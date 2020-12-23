module Server where

import qualified Codec.Serialise as Serialise
import Core
import qualified Data.Aeson as Aeson
import qualified Github
import qualified JobHandler
import qualified Network.HTTP.Types as HTTP.Types
import qualified Network.Wai.Middleware.Cors as Cors
import RIO
import qualified RIO.Map as Map
import qualified RIO.NonEmpty as NonEmpty
import qualified Web.Scotty as Scotty

data Config = Config
  { port :: Int
  }

run :: Config -> JobHandler.Service -> IO ()
run config handler =
  Scotty.scotty config.port do
    Scotty.middleware Cors.simpleCors

    Scotty.post "/agent/pull" do
      cmd <- Scotty.liftAndCatchIO do
        handler.dispatchCmd

      Scotty.raw $ Serialise.serialise cmd

    Scotty.post "/agent/send" do
      msg <- Serialise.deserialise <$> Scotty.body

      Scotty.liftAndCatchIO do
        handler.processMsg msg

      Scotty.json ("message processed" :: Text)

    Scotty.post "/webhook/github" do
      body <- Scotty.body

      number <- Scotty.liftAndCatchIO do
        info <- Github.parsePushEvent (toStrictBytes body)
        pipeline <- Github.fetchRemotePipeline info
        let step = Github.createCloneStep info
        handler.queueJob info $
          pipeline
            { steps = NonEmpty.cons step pipeline.steps
            }

      Scotty.json $ Core.buildNumberToInt number

    Scotty.get "/build/:number" do
      number <- BuildNumber <$> Scotty.param "number"

      job <-
        Scotty.liftAndCatchIO
          (handler.findJob number)
          >>= \case
            Nothing -> Scotty.raiseStatus HTTP.Types.status404 "Build not found"
            Just j -> pure j

      Scotty.json $ jobToJson number job

    Scotty.get "/build/:number/step/:step/logs" do
      number <- BuildNumber <$> Scotty.param "number"
      step <- StepName <$> Scotty.param "step"

      log <- Scotty.liftAndCatchIO $ handler.fetchLogs number step
      Scotty.raw $ fromStrictBytes $ fromMaybe "" log

    Scotty.get "/build" do
      jobs <- Scotty.liftAndCatchIO do
        handler.latestJobs

      Scotty.json $ jobs <&> \(number, job) -> jobToJson number job

jobToJson :: BuildNumber -> JobHandler.Job -> Aeson.Value
jobToJson number job =
  Aeson.object
    [ ("number", Aeson.toJSON $ Core.buildNumberToInt number),
      ("state", Aeson.toJSON $ jobStateToText job.state),
      ("info", Aeson.toJSON job.info),
      ("steps", Aeson.toJSON steps)
    ]
  where
    build = case job.state of
      JobHandler.JobQueued -> Nothing
      JobHandler.JobAssigned -> Nothing
      JobHandler.JobScheduled b -> Just b
    steps =
      job.pipeline.steps <&> \step ->
        Aeson.object
          [ ("name", Aeson.String $ stepNameToText step.name),
            ( "state",
              Aeson.String $ case build of
                Just b -> stepStateToText b step
                Nothing -> "ready"
            )
          ]

jobStateToText :: JobHandler.JobState -> Text
jobStateToText = \case
  JobHandler.JobQueued -> "queued"
  JobHandler.JobAssigned -> "assigned"
  JobHandler.JobScheduled b -> case b.state of
    BuildReady -> "ready"
    BuildRunning _ -> "running"
    BuildFinished result -> case result of
      BuildSucceeded -> "succeeded"
      BuildFailed -> "failed"
      BuildUnexpectedState _ -> "unexpectedstate"

stepStateToText :: Build -> Step -> Text
stepStateToText build step =
  case build.state of
    BuildRunning s ->
      if s.step == step.name
        then "running"
        else stepNotRunning
    _ ->
      stepNotRunning
  where
    stepNotRunning = case Map.lookup step.name build.completedSteps of
      Just StepSucceeded -> "succeeded"
      Just (StepFailed _) -> "failed"
      Nothing -> case build.state of
        BuildFinished _ -> "skipped"
        _ -> "ready"
