module Socket where

import qualified Network.HTTP.Client as Client
import qualified Network.HTTP.Client.Internal as Client.Internal
import qualified Network.Socket as S
import qualified Network.Socket.ByteString as SBS
import RIO

newManager :: FilePath -> IO Client.Manager
newManager fp =
  Client.newManager $
    Client.defaultManagerSettings
      { Client.managerRawConnection = pure makeSocket
      }
  where
    makeSocket _ _ _ = do
      s <- S.socket S.AF_UNIX S.Stream S.defaultProtocol
      S.connect s (S.SockAddrUnix fp)
      Client.Internal.makeConnection
        (SBS.recv s 8096)
        (SBS.sendAll s)
        (S.close s)