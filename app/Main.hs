{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import qualified Data.Text as Text
import Control.Lens
import Control.Arrow ((&&&))
import Control.Exception (try)
import Control.Monad (void, forever, when)
import Control.Monad.Except (runExceptT)
import Data.String.Conversions  (convertString)
import Data.Text.Strict.Lens
import URI.ByteString
import Data.Aeson (ToJSON, encode, decode)
import Wuss (runSecureClient)
import qualified Network.WebSockets as Sock
import Network.HTTP.Simple
import Network.HTTP.Conduit
import Control.Concurrent (forkIO)
import Control.Concurrent.Chan
import Control.Concurrent.MVar

import Network.Linklater
import Network.Linklater.Types

import Types
import SlackOAuth

import qualified System.Environment as Env

voila :: URI -> Chan Speech -> IO (Chan Bytes)
voila uri outbox =
  case (uri ^? authorityL . _Just . authorityHostL . hostBSL . utf8 . unpacked,
        uri ^? pathL . utf8 . unpacked) of
    (Just host, Just path) -> do
      chan <- newChan
      runSecureClient host 443 path (consumer chan)
      return chan
    _ ->
      error ("invalid url" ++ show uri)
  where
    consumer chan conn = do
      void $ forkIO (forever worker)
      void $ forkIO (forever listener)
      where
        worker = do
          msg <- Sock.receiveData conn
          writeChan chan msg
        listener = do
          speech <- readChan outbox
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

checkAuthStatus :: String -> IO Bool
checkAuthStatus token = do
  initReq <- parseRequest "https://slack.com/api/auth.test"
  let req = setRequestQueryString [("token", convertString <$> Just token)]
                                  (initReq { method = "POST" })
  response <- responseBody <$> httpLBS req
  let authTest = decode response :: Maybe AuthTest
  putStrLn (show authTest)
  case authTest of
    Nothing -> return False
    Just at -> return (ok at)

readToken :: IO String
readToken = readFile "./token.txt"

tryReadToken :: IO String
tryReadToken = do
  tokenOrExc <- try $ readToken
  case tokenOrExc of
    Left (e :: IOError) -> do
      runWebServer
      readToken
    Right token -> do
      authResult <- checkAuthStatus token
      when (not authResult) runWebServer; token <- readToken
      return token

main :: IO ()
main = void $ do
  apiToken <- tryReadToken
  runBots apiToken

runBots :: String -> IO ()
runBots apiToken = do
  outbox <- newChan
  uriResult <- runExceptT (startRTM $ APIToken $ Text.pack apiToken)
  case uriResult of
    Left e -> do
      error ("Request error" ++ show e)
    Right uri -> do
      inbox <- voila uri outbox
      putStrLn "Running jazzBot"
      jazzBot inbox outbox
      sinkChan inbox

sinkChan :: Chan Bytes -> IO ()
sinkChan originalChan =
  (void . forever) $ readChan originalChan
