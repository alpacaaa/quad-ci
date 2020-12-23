module Cli where

import qualified Agent
import qualified Docker
import qualified JobHandler.Memory
import RIO
import qualified Runner
import qualified Server
import qualified System.Log.Logger as Logger
import qualified UI.Butcher.Monadic as Butcher

defaultPort :: Int
defaultPort = 9000

defaultEndpoint :: String
defaultEndpoint = "http://localhost:" <> show defaultPort

data Command
  = StartServer Server.Config
  | StartAgent Agent.Config

runCommand :: Command -> IO ()
runCommand = \case
  StartServer config -> do
    Logger.infoM "quad.server" "Server starting..."
    handler <- JobHandler.Memory.createService
    Server.run config handler
  StartAgent config -> do
    Logger.infoM "quad.agent" "Agent starting..."
    docker <- Docker.createService
    runner <- Runner.createService docker
    Agent.run config runner

main :: IO ()
main = Butcher.mainFromCmdParserWithHelpDesc $ \helpDesc -> do
  Butcher.addCmdSynopsis "Quad CI command line utility"
  Butcher.addHelpCommand2 helpDesc

  Butcher.addCmd "start-server" do
    Butcher.addCmdSynopsis "Start server node"
    port <-
      Butcher.addParamString "PORT" $
        Butcher.paramHelpStr "Server port"
          <> Butcher.paramDefault (show defaultPort)
    Butcher.addCmdImpl $
      case readMaybe port of
        Nothing -> throwString "Port must be a number"
        Just p -> do
          let config = Server.Config {port = p}
          runCommand $ StartServer config

  Butcher.addCmd "start-agent" do
    Butcher.addCmdSynopsis "Start agent node"
    endpoint <-
      Butcher.addParamString "ENDPOINT" $
        Butcher.paramHelpStr "Server endpoint"
          <> Butcher.paramSuggestions [defaultEndpoint]
          <> Butcher.paramDefault defaultEndpoint

    Butcher.addCmdImpl do
      let config = Agent.Config {endpoint = endpoint}
      runCommand $ StartAgent config
