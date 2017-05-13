module Main where

import Web.Slack
import Web.Slack.Message
import System.Environment (lookupEnv)
import Data.Maybe (fromMaybe)
import Control.Applicative
import Data.Text (pack)

import Lib

config :: String -> SlackConfig
config apiToken = SlackConfig { _slackApiToken = apiToken }

echoBot :: SlackBot ()
echoBot (Message cid _ msg _ _ _) = sendMessage cid (pack "OK")
echoBot _ = return ()

main :: IO ()
main = do
  apiToken <- fromMaybe (error "SLACK_API_TOKEN not set") <$> lookupEnv "SLACK_API_TOKEN"
  runBot (config apiToken) echoBot ()
