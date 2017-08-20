{-# LANGUAGE OverloadedStrings #-}

module Main where

import Control.Monad.Except (runExceptT)

import Data.Text
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.Time.LocalTime (TimeZone, getCurrentTimeZone, utcToZonedTime)
import Data.Time.Format (formatTime, defaultTimeLocale)
import Text.Printf (printf)

import Network.Wai.Handler.Warp (run)
import Network.Linklater

convertTime :: UTCTime -> TimeZone -> String
convertTime time tz =
  let timeJST = utcToZonedTime tz time in
  formatTime defaultTimeLocale "%Y/%m/%d %H:%M" timeJST

getCurrentTimeString :: IO String
getCurrentTimeString = convertTime <$> getCurrentTime <*> getCurrentTimeZone

prompt :: IO ()
prompt = putStr "Enter command > "

processStart :: String -> IO ()
processStart timeStr = putStrLn $ printf "Started at %s" timeStr

processFinish :: String -> IO ()
processFinish timeStr = putStrLn $ printf "Finished at %s" timeStr

sendCommand :: String -> String -> IO ()
sendCommand command timeStr =
  case command of
    "start"  -> processStart timeStr
    "finish" -> processFinish timeStr
    _ -> putStrLn $ printf "Do not know how to process command: %s" command

timestamp :: Command -> IO Text
timestamp (Command command user_ channel (Just query)) = do
  putStrLn $ printf "Command: %s" (show command)
  let config = Config $ "SLACK_URL"
  let message = SimpleMessage (EmojiIcon $ "gift") "Bot" channel "start"
  runExceptT $ say message config
  return "ok"

main :: IO ()
main = do
  putStrLn $ printf "Listening on port %d" port
  run port (slashSimple timestamp)
    where port = 8833
