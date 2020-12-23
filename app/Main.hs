module Main where

import qualified Cli
import RIO
import qualified System.Log.Formatter as Logger
import qualified System.Log.Handler as Handler
import qualified System.Log.Handler.Simple as Logger
import qualified System.Log.Logger as Logger

initLogger :: IO ()
initLogger = do
  logger <- Logger.getRootLogger
  handler <- Logger.streamHandler stdout Logger.INFO
  let formatter = Logger.simpleLogFormatter "[$time : $loggername : $prio] $msg"

  Logger.saveGlobalLogger $
    logger
      & Logger.setHandlers [Handler.setFormatter handler formatter]
      & Logger.setLevel Logger.INFO

main :: IO ()
main = do
  initLogger
  Cli.main