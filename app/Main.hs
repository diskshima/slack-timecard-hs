{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Arrow           ((&&&))
import           Control.Concurrent.Chan
import           Control.Concurrent.MVar
import           Control.Exception       (try)
import           Control.Lens
import           Control.Monad           (forever, unless, void, when)
import           Control.Monad.Except    (runExceptT)
import           Data.String.Conversions (convertString)
import qualified Data.Text               as Text
import           Data.Text.Strict.Lens
import           Network.HTTP.Conduit
import           Network.Linklater
import           Network.Linklater.Types
import           SlackOAuth
import           Types
import           URI.ByteString
import           SlackPipe

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
      inbox <- chanStarter uri outbox
      jazzBot inbox outbox
      sinkChan inbox

sinkChan :: Chan Bytes -> IO ()
sinkChan originalChan =
  (void . forever) $ readChan originalChan

main :: IO ()
main = void $ do
  apiToken <- tryReadToken
  runBots apiToken
