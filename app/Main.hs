module Main where

import Data.Time.Clock
import Data.Time.LocalTime
import Data.Time.Format
import Text.Printf

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


main :: IO ()
main = do
  prompt
  command <- getLine
  timeStr <- getCurrentTimeString
  sendCommand command timeStr
