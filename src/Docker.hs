module Docker where

import qualified Codec.Serialise as Serialise
import Data.Aeson ((.:))
import qualified Data.Aeson as Aeson
import qualified Data.Aeson.Types as Aeson.Types
import qualified Data.Time.Clock.POSIX as Time
import qualified Network.HTTP.Simple as HTTP
import RIO
import qualified RIO.Text as Text
import qualified RIO.Text.Partial as Text.Partial
import qualified Socket

data Service = Service
  { createContainer :: CreateContainerOptions -> IO ContainerId,
    startContainer :: ContainerId -> IO (),
    containerStatus :: ContainerId -> IO ContainerStatus,
    createVolume :: IO Volume,
    fetchLogs :: FetchLogsOptions -> IO ByteString,
    pullImage :: Image -> IO ()
  }

createService :: IO Service
createService = do
  -- Init manager once
  manager <- Socket.newManager "/var/run/docker.sock"

  -- Make Request
  let makeReq :: RequestBuilder
      makeReq path =
        HTTP.defaultRequest
          & HTTP.setRequestPath (encodeUtf8 $ "/v1.40" <> path)
          & HTTP.setRequestManager manager

  pure
    Service
      { createContainer = createContainer_ makeReq,
        startContainer = startContainer_ makeReq,
        containerStatus = containerStatus_ makeReq,
        createVolume = createVolume_ makeReq,
        fetchLogs = fetchLogs_ makeReq,
        pullImage = pullImage_ makeReq
      }

type RequestBuilder = Text -> HTTP.Request

newtype ContainerExitCode = ContainerExitCode Int
  deriving (Eq, Show, Generic, Serialise.Serialise)

exitCodeToInt :: ContainerExitCode -> Int
exitCodeToInt (ContainerExitCode code) = code

data Image = Image {name :: Text, tag :: Text}
  deriving (Eq, Show, Generic, Serialise.Serialise)

imageToText :: Image -> Text
imageToText image = image.name <> ":" <> image.tag

instance Aeson.FromJSON Image where
  parseJSON = Aeson.withText "parse-image" $ \image -> do
    case Text.Partial.splitOn ":" image of
      [name] ->
        pure $ Image {name = name, tag = "latest"}
      [name, tag] ->
        pure $ Image {name = name, tag = tag}
      _ ->
        fail $ "Image has too many colons " <> Text.unpack image

newtype ContainerId = ContainerId Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

containerIdToText :: ContainerId -> Text
containerIdToText (ContainerId c) = c

data ContainerStatus
  = ContainerRunning
  | ContainerExited ContainerExitCode
  | ContainerOther Text
  deriving (Eq, Show)

newtype Volume = Volume Text
  deriving (Eq, Show, Generic, Serialise.Serialise)

volumeToText :: Volume -> Text
volumeToText (Volume v) = v

data CreateContainerOptions = CreateContainerOptions
  { image :: Image,
    script :: Text,
    volume :: Volume
  }

createContainer_ :: RequestBuilder -> CreateContainerOptions -> IO ContainerId
createContainer_ makeReq options = do
  let image = imageToText options.image
  let bind = volumeToText options.volume <> ":/app"

  let body =
        Aeson.object
          [ ("Image", Aeson.toJSON image),
            ("Tty", Aeson.toJSON True),
            ("Labels", Aeson.object [("quad", "")]),
            ("Entrypoint", Aeson.toJSON [Aeson.String "/bin/sh", "-c"]),
            ("Cmd", "echo \"$QUAD_SCRIPT\" | /bin/sh"),
            ("Env", Aeson.toJSON ["QUAD_SCRIPT=" <> options.script]),
            ("WorkingDir", "/app"),
            ("HostConfig", Aeson.object [("Binds", Aeson.toJSON [bind])])
          ]

  let req =
        makeReq "/containers/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  let parser = Aeson.withObject "create-container" $ \o -> do
        cId <- o .: "Id"
        pure $ ContainerId cId

  res <- HTTP.httpBS req
  parseResponse res parser

startContainer_ :: RequestBuilder -> ContainerId -> IO ()
startContainer_ makeReq container = do
  let path = "/containers/" <> containerIdToText container <> "/start"
  let req =
        makeReq path
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

containerStatus_ :: RequestBuilder -> ContainerId -> IO ContainerStatus
containerStatus_ makeReq container = do
  let parser = Aeson.withObject "container-inspect" $ \o -> do
        state <- o .: "State"
        status <- state .: "Status"
        case status of
          "running" -> pure ContainerRunning
          "exited" -> do
            code <- state .: "ExitCode"
            pure $ ContainerExited (ContainerExitCode code)
          other -> pure $ ContainerOther other

  let req =
        makeReq $
          "/containers/" <> containerIdToText container <> "/json"

  res <- HTTP.httpBS req
  parseResponse res parser

createVolume_ :: RequestBuilder -> IO Volume
createVolume_ makeReq = do
  let body =
        Aeson.object
          [ ("Labels", Aeson.object [("quad", "")])
          ]

  let req =
        makeReq "/volumes/create"
          & HTTP.setRequestMethod "POST"
          & HTTP.setRequestBodyJSON body

  let parser = Aeson.withObject "create-volume" $ \o -> do
        name <- o .: "Name"
        pure $ Volume name

  res <- HTTP.httpBS req
  parseResponse res parser

data FetchLogsOptions = FetchLogsOptions
  { container :: ContainerId,
    since :: Time.POSIXTime,
    until :: Time.POSIXTime
  }

fetchLogs_ :: RequestBuilder -> FetchLogsOptions -> IO ByteString
fetchLogs_ makeReq options = do
  let timestampToText t = tshow (round t :: Int)
  let url =
        "/containers/"
          <> containerIdToText options.container
          <> "/logs?stdout=true&stderr=true&since="
          <> timestampToText options.since
          <> "&until="
          <> timestampToText options.until

  res <- HTTP.httpBS $ makeReq url
  pure $ HTTP.getResponseBody res

pullImage_ :: RequestBuilder -> Image -> IO ()
pullImage_ makeReq image = do
  let url =
        "/images/create?tag="
          <> image.tag
          <> "&fromImage="
          <> image.name

  let req =
        makeReq url
          & HTTP.setRequestMethod "POST"

  void $ HTTP.httpBS req

parseResponse ::
  HTTP.Response ByteString ->
  (Aeson.Value -> Aeson.Types.Parser a) ->
  IO a
parseResponse res parser = do
  let result = do
        value <- Aeson.eitherDecodeStrict (HTTP.getResponseBody res)
        Aeson.Types.parseEither parser value

  case result of
    Left e -> throwString e
    Right status -> pure status
