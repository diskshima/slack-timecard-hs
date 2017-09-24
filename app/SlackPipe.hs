module SlackPipe where

import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan (Chan, newChan, readChan, writeChan)
import           Control.Lens
import           Control.Monad           (forever, void)
import           Data.Aeson              (encode)
import           Data.Text.Strict.Lens
import           Network.WebSockets      (Connection, receiveData, sendTextData)
import           Types
import           URI.ByteString
import           Wuss                    (runSecureClient)

chanStarter :: URI -> Chan Speech -> IO (Chan Bytes)
chanStarter uri outbox =
  case (uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked,
        uri ^? pathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      inbox <- newChan
      putStrLn $ "Connecting to " ++ host ++ path
      forkIO $ runSecureClient host 443 path (consumer inbox)
      return inbox
    _ ->
      error ("invalid url" ++ show uri)
  where
    consumer inbox conn = do
      void . forkIO . forever $ socketToChan conn inbox
      forever $ chanToSocket conn outbox

socketToChan :: Connection -> Chan Bytes -> IO ()
socketToChan conn inbox = do
  putStrLn "Waiting on WebSocket for any data."
  msg <- receiveData conn
  putStrLn $ "Message read: " ++ show msg
  writeChan inbox msg

chanToSocket :: Connection -> Chan Speech -> IO ()
chanToSocket conn outbox = do
  putStrLn "Waiting for message to send out"
  speech <- readChan outbox
  putStrLn $ "Sending message: " ++ show speech
  sendTextData conn (encode (Speech' speech 1))
