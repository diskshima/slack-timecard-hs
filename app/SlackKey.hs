{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}

module SlackKey where

import           Prelude as P
import           Data.String.Conversions (convertString)
import           Data.Text               (Text, pack)
import           Network.OAuth.OAuth2
import           System.Environment
import           URI.ByteString
import           URI.ByteString.QQ

readPort :: URI -> Maybe Int
readPort uri = do
  case uriAuthority uri of
    Nothing -> Nothing
    Just auth -> do
      case authorityPort auth of
        Nothing -> Nothing
        Just port -> Just $ portNumber port

readCallbackURI :: IO URI
readCallbackURI = do
  callbackURI <- readEnvVar "SLACK_CALLBACK_URI"
  case parseURILax callbackURI of
    Nothing  -> P.error "Could not parse environment variable SLACK_CALLBACK_URI"
    Just uri -> return uri

readEnvVar :: String -> IO Text
readEnvVar varName = do
  Just value <- lookupEnv varName
  return $ pack value

getSlackKey :: IO OAuth2
getSlackKey = do
  clientId <- convertString <$> readEnvVar "SLACK_OAUTH_CLIENT_ID"
  clientSecret <- convertString <$>  readEnvVar "SLACK_OAUTH_CLIENT_SECRET"
  callbackURI <- readCallbackURI
  return OAuth2 { oauthClientId = clientId
                , oauthClientSecret = clientSecret
                , oauthCallback = Just callbackURI
                , oauthOAuthorizeEndpoint = [uri|https://slack.com/oauth/authorize|]
                , oauthAccessTokenEndpoint = [uri|https://slack.com/api/oauth.access|]
                }

parseURILax :: Text -> Maybe URI
parseURILax uriText = do
  case parseURI laxURIParserOptions (convertString uriText) of
    Left e    -> Nothing
    Right uri -> Just uri
