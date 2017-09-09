{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module SlackKey where

import qualified Data.ByteString      as BS
import           Data.Text            (Text, pack)
import           Network.OAuth.OAuth2
import           System.Environment
import           URI.ByteString.QQ

readEnvVar :: String -> IO Text
readEnvVar varName = do
  Just value <- lookupEnv varName
  return $ pack value

getSlackKey :: IO OAuth2
getSlackKey = do
  clientId <- readEnvVar "SLACK_OAUTH_CLIENT_ID"
  clientSecret <- readEnvVar "SLACK_OAUTH_CLIENT_SECRET"
  return OAuth2 { oauthClientId = clientId
                , oauthClientSecret = clientSecret
                , oauthCallback = Just [uri|http://127.0.0.1:9988/oauthCallback|]
                , oauthOAuthorizeEndpoint = [uri|https://slack.com/oauth/authorize|]
                , oauthAccessTokenEndpoint = [uri|https://slack.com/api/oauth.access|]
         }
