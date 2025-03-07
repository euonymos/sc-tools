{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}

module Convex.TxMod.Cli (
  runMain,
) where

import Blammo.Logging.Simple (
  Message ((:#)),
  MonadLogger,
  MonadLoggerIO,
  WithLogger (..),
  logError,
  logInfo,
  logWarn,
  runLoggerLoggingT,
 )
import Blockfrost.Client.Core (BlockfrostError)
import Cardano.Api (TxId)
import Control.Lens (view)
import Control.Monad (when)

import Control.Monad.Except (ExceptT, MonadError, runExceptT, throwError)
import Control.Monad.IO.Class (MonadIO (..))
import Control.Monad.Reader (
  MonadReader,
  ReaderT,
  asks,
  runReaderT,
 )
import Convex.Blockfrost qualified
import Convex.Blockfrost.Orphans ()
import Convex.Blockfrost.Types (
  DecodingError (..),
 )
import Convex.ResolvedTx (ResolvedTx (..))
import Convex.ResolvedTx qualified
import Convex.TxMod.Command (
  ResolvedTxInput (..),
  TxModCommand (..),
  parseCommand,
 )
import Convex.TxMod.Env (Env)
import Convex.TxMod.Env qualified as Env
import Convex.TxMod.Logging qualified as L
import Convex.Utils (liftEither, mapError, requiredTxIns, txnUtxos)
import Data.Aeson qualified
import Data.Aeson.Encode.Pretty (encodePretty)
import Data.ByteString as BS (readFile)
import Data.ByteString.Base16 qualified as Base16
import Data.ByteString.Lazy.Char8 qualified as LBS

import Blockfrost.Types (TransactionCBOR (TransactionCBOR), _transactionCBORCbor)
import Cardano.Api qualified as C
import Data.Aeson (decodeStrictText)
import Data.Bifunctor (second)
import Data.Data (Proxy (Proxy))
import Data.Map qualified as Map
import Data.Maybe (fromJust, fromMaybe)
import Data.Set qualified as Set
import Data.String (IsString (fromString))
import Data.Text qualified as Text
import Data.Text.Encoding qualified as Text.Encoding
import Data.Text.IO qualified as Text.IO
import Options.Applicative (
  customExecParser,
  disambiguate,
  helper,
  idm,
  info,
  prefs,
  showHelpOnEmpty,
  showHelpOnError,
 )

runMain :: IO ()
runMain = do
  customExecParser
    (prefs $ disambiguate <> showHelpOnEmpty <> showHelpOnError)
    (info (helper <*> parseCommand) idm)
    >>= runCommand

runCommand :: TxModCommand -> IO ()
runCommand com = do
  env <- Env.loadEnv
  result <- runTxModApp env $ case com of
    Download txId output -> downloadTx txId output
    Graph input output -> graph input output
    Bulk input output -> bulk input output
  case result of
    Left err -> runLoggerLoggingT env $ do
      logError (fromString $ show err)
    Right a -> pure a

downloadTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => TxId -> Maybe FilePath -> m ()
downloadTx txId filePath = resolveTx txId >>= writeTx filePath

graph :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => [ResolvedTxInput] -> Maybe FilePath -> m ()
graph inputs outFile = do
  when (null inputs) $ logWarn "No resolved transactions provided, graph will be empty"
  traverse getTx inputs >>= writeGraph outFile

bulk :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => FilePath -> Maybe FilePath -> m ()
bulk txDump outFile = do
  txLines <- Text.lines . Text.Encoding.decodeUtf8 <$> liftIO (BS.readFile txDump)
  allTx <- mapM (decodeTransactionCBOR' . TransactionCBOR) txLines
  let m :: Map.Map TxId (C.Tx C.ConwayEra) =
        Map.fromList (fmap (\t -> let C.Tx b _ = t in (C.getTxId b, t)) allTx)
  let resolved :: [ResolvedTx] =
        fmap
          ( \t ->
              let
                (C.Tx (C.TxBody bodyContent) _) = t
                txIns = requiredTxIns bodyContent
                utxos = (C.UTxO . Map.fromList) $ fmap (\txIn -> (txIn, lookupOutput m txIn)) (Set.toList txIns)
               in
                ResolvedTx{rtxTransaction = t, rtxInputs = C.unUTxO utxos}
          )
          allTx

  writeGraph outFile resolved
 where
  lookupOutput :: (C.IsShelleyBasedEra era) => Map.Map TxId (C.Tx era) -> C.TxIn -> C.TxOut C.CtxUTxO era
  lookupOutput m (C.TxIn txId (C.TxIx txIx)) =
    fromMaybe seedOutput $ do
      tx <- Map.lookup txId m
      let utxos = second C.toCtxUTxOTxOut <$> txnUtxos tx
      pure $ snd $ utxos !! fromIntegral txIx
  -- trace (unpack $ TL.toStrict $ encodeToLazyText output) output

  seedOutput :: (C.IsShelleyBasedEra era) => C.TxOut C.CtxUTxO era
  seedOutput =
    fromJust $
      decodeStrictText $
        Text.pack
          "{\"address\":\"addr_test1qryvgass5dsrf2kxl3vgfz76uhp83kv5lagzcp29tcana68ca5aqa6swlq6llfamln09tal7n5kvt4275ckwedpt4v7q48uhex\",\"value\":{\"lovelace\":10000000000}}"

-- original decodeTransactionCBOR with another MonadError parameter,
-- can be handled with mapError
decodeTransactionCBOR' :: forall era m. (MonadError AppError m, C.IsShelleyBasedEra era) => TransactionCBOR -> m (C.Tx era)
decodeTransactionCBOR' TransactionCBOR{_transactionCBORCbor} =
  either (throwError . DecodingErr . Base16DecodeError) pure (Base16.decode $ Text.Encoding.encodeUtf8 _transactionCBORCbor)
    >>= either (throwError . DecodingErr . CBORError) pure . C.deserialiseFromCBOR (C.proxyToAsType $ Proxy @(C.Tx era))

newtype TxModApp a = TxModApp {unTxModApp :: ReaderT Env (ExceptT AppError IO) a}
  deriving newtype (Monad, Applicative, Functor, MonadIO, MonadReader Env, MonadError AppError)
  deriving
    (MonadLogger, MonadLoggerIO)
    via (WithLogger Env (ExceptT AppError IO))

data AppError
  = DecodingErr DecodingError
  | BlockfrostErr BlockfrostError
  | FileDecodeErr String
  deriving stock (Show)

runTxModApp :: Env -> TxModApp a -> IO (Either AppError a)
runTxModApp env TxModApp{unTxModApp} = runExceptT (runReaderT unTxModApp env)

writeTx :: (MonadLoggerIO m) => Maybe FilePath -> ResolvedTx -> m ()
writeTx = \case
  Nothing -> \tx -> do
    logInfo "Writing tx to stdout"
    liftIO . LBS.putStrLn . encodePretty $ tx
  Just fp -> \tx -> do
    logInfo $ "Writing tx to file" :# [L.txFile fp]
    liftIO . LBS.writeFile fp . encodePretty $ tx

resolveTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => TxId -> m ResolvedTx
resolveTx txId = do
  logInfo $ "Downloading tx" :# [L.txId txId]
  project <- asks (view Env.blockfrostProject)
  liftEither BlockfrostErr (mapError DecodingErr (Convex.Blockfrost.evalBlockfrostT project (Convex.Blockfrost.resolveTx txId)))

loadTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => FilePath -> m ResolvedTx
loadTx fp = do
  logInfo $ "Reading tx from file" :# [L.txFile fp]
  liftEither FileDecodeErr (Data.Aeson.eitherDecode <$> liftIO (LBS.readFile fp))

getTx :: (MonadLoggerIO m, MonadReader Env m, MonadError AppError m) => ResolvedTxInput -> m ResolvedTx
getTx (ResolvedTxInput k) = either loadTx resolveTx k

writeGraph :: (MonadLoggerIO m) => Maybe FilePath -> [ResolvedTx] -> m ()
writeGraph = \case
  Nothing -> \tx -> do
    logInfo "Writing graph to stdout"
    liftIO . Text.IO.putStrLn $ Convex.ResolvedTx.dot tx
  Just fp -> \tx -> do
    logInfo $ "Writing graph to file" :# [L.dotGraphFile fp]
    liftIO . Convex.ResolvedTx.dotFile fp $ tx
