-- | Command type and parser
module Convex.TxMod.Command (
  TxModCommand (..),
  ResolvedTxInput (..),
  parseCommand,
) where

import Cardano.Api (TxId)
import Options.Applicative (
  CommandFields,
  Mod,
  Parser,
  argument,
  command,
  fullDesc,
  help,
  info,
  long,
  many,
  metavar,
  optional,
  progDesc,
  short,
  str,
  strOption,
  subparser,
  (<|>),
 )

data TxModCommand
  = -- | Download the transaction from blockfrost and print it to stdout, or write it to the file if a file is provided
    Download TxId (Maybe FilePath)
  | -- | visualise the transaction
    Graph [ResolvedTxInput] (Maybe FilePath)
  | -- | load local file with a bunch of transaction on visualize them
    Bulk FilePath (Maybe FilePath)

parseCommand :: Parser TxModCommand
parseCommand = subparser $ mconcat [parseDownload, parseGraph, parseBulk]

parseDownload :: Mod CommandFields TxModCommand
parseDownload =
  command "download" $
    info (Download <$> parseTxId <*> optional parseTxOutFile) (fullDesc <> progDesc "Download a fully resolved transaction from blockfrost")

parseGraph :: Mod CommandFields TxModCommand
parseGraph =
  command "graph" $
    info (Graph <$> many parseResolvedTxInput <*> optional parseGraphOutFile) (fullDesc <> progDesc "Generate a dot graph (graphviz) from a fully resolved transaction")

parseBulk :: Mod CommandFields TxModCommand
parseBulk =
  command "bulk" $
    info (Bulk <$> parseTxDumpFile <*> optional parseGraphOutFile) (fullDesc <> progDesc "Load a bunch of txs from file and visualize them")

parseTxId :: Parser TxId
parseTxId =
  argument
    str
    (metavar "TX_ID" <> help "The transaction ID")

parseTxOutFile :: Parser FilePath
parseTxOutFile = strOption (long "out.file" <> short 'o' <> help "File to write the fully resolved transaction to")

parseTxInFile :: Parser FilePath
parseTxInFile = strOption (long "in.file" <> short 'f' <> help "JSON file with the fully resolved transaction")

parseTxDumpFile :: Parser FilePath
parseTxDumpFile = strOption (long "txs.out" <> short 'f' <> help "Txs dump file: each line is serilized CBOR, no trailing \\n")

parseGraphOutFile :: Parser FilePath
parseGraphOutFile = strOption (long "out.file" <> short 'o' <> help "File to write the dot graph to")

-- | Resolved tx either provided as a file path or as a transaction ID
newtype ResolvedTxInput = ResolvedTxInput (Either FilePath TxId)

parseResolvedTxInput :: Parser ResolvedTxInput
parseResolvedTxInput =
  ResolvedTxInput <$> (fmap Left parseTxInFile <|> fmap Right parseTxId)
