{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow           ((&&&))
import           Control.Concurrent      (forkIO)
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception       (try)
import           Control.Lens
import           Control.Monad           (forever, unless, void, when)
import           Control.Monad.Except    (runExceptT)
import           Data.Aeson              (ToJSON, encode)
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           Data.Text.Strict.Lens
import           Network.HTTP.Conduit
import           Network.Linklater
import           Network.Linklater.Types
import qualified Network.WebSockets      as Sock
import           SlackOAuth
import           Types
import           URI.ByteString
import Control.Concurrent (forkIO)
import Control.Monad (forever, void)
import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as BL
import Data.ByteString.Lazy (toStrict)
import Data.Text (Text, pack)
import Network.Connection (Connection, ConnectionParams (..), TLSSettings (..),
    connectionGetChunk, connectionPut, connectTo, initConnectionContext)
import Network.Socket (PortNumber (..))
import Network.WebSockets (ClientApp, ConnectionOptions, Headers,
    defaultConnectionOptions, receiveData, runClientWithStream, sendClose,
    sendTextData)
import Network.WebSockets.Stream (makeStream)
-- import           Wuss                    (runSecureClient)

import qualified System.Environment as Env

runSecureClient :: String -> PortNumber -> String -> ClientApp a -> IO a
runSecureClient host port path app = do
    context <- initConnectionContext
    connection <- connectTo context (connectionParams host port)
    stream <- makeStream (reader connection) (writer connection)
    runClientWithStream stream host path connectionOptions headers app

connectionParams :: String -> PortNumber -> ConnectionParams
connectionParams host port = ConnectionParams
    { connectionHostname = host
    , connectionPort = port
    , connectionUseSecure = Just tlsSettings
    , connectionUseSocks = Nothing
    }

tlsSettings :: TLSSettings
tlsSettings = TLSSettingsSimple
    { settingDisableCertificateValidation = False
    , settingDisableSession = False
    , settingUseServerName = False
    }

reader :: Connection -> IO (Maybe BS.ByteString)
reader connection = fmap Just (connectionGetChunk connection)

writer :: Connection -> Maybe BL.ByteString -> IO ()
writer connection = maybe (return ()) (connectionPut connection . toStrict)

connectionOptions :: ConnectionOptions
connectionOptions = defaultConnectionOptions

headers :: Headers
headers = []

voila :: URI -> Chan Speech -> IO (Chan Bytes)
voila uri outbox =
  case (uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked,
        uri ^? pathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      chan <- newChan
      putStrLn $ "Connecting to " ++ host ++ path
      runSecureClient host 443 path (consumer chan)
      return chan
    _ ->
      error ("invalid url" ++ show uri)
  where
    consumer chan conn = do
      void . forkIO . forever $ worker
      void . forkIO . forever $ listener
      where
        worker = do
          putStrLn "Worker: Waiting to read any messages"
          msg <- Sock.receiveData conn
          putStrLn $ "Message read: " ++ (show msg)
          writeChan chan msg
        listener = do
          putStrLn "Listener: waiting for message to send out"
          speech <- readChan outbox
          putStrLn $ "Sending message: " ++ (show speech)
          Sock.sendTextData conn (encode (Speech' speech 1))

jazzBot :: Chan Bytes -> Chan Speech -> IO ()
jazzBot inbox outbox = do
  countDB <- newMVar (0 :: Int)
  withInbox inbox $ \line_ ->
    when (Text.isInfixOf ":raised_hands:" (line_ ^. truth)) $ do
      let update = (`mod` 3) . (+ 1) &&& Types.id
      count <- modifyMVar countDB (return . update)
      when (count == 2) $
        writeChan outbox (Speech line_ "JAZZ HANDS")

readToken :: IO String
readToken = readFile "./token.txt"

tryReadToken :: IO String
tryReadToken = do
  tokenOrExc <- try readToken
  case tokenOrExc of
    Left (e :: IOError) -> do
      runWebServer
      readToken
    Right token -> do
      authResult <- checkAuthStatus token
      unless authResult runWebServer
      readToken

runBots :: String -> IO ()
runBots apiToken = do
  outbox <- newChan
  uriResult <- runExceptT (startRTM $ APIToken $ Text.pack apiToken)
  case uriResult of
    Left e ->
      error ("Request error" ++ show e)
    Right uri -> do
      inbox <- voila uri outbox
      jazzBot inbox outbox
      sinkChan inbox

sinkChan :: Chan Bytes -> IO ()
sinkChan originalChan =
  (void . forever) $ readChan originalChan

main :: IO ()
main = void $ do
  apiToken <- tryReadToken
  runBots apiToken
