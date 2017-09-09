{-# LANGUAGE FlexibleContexts  #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE QuasiQuotes       #-}
{-# LANGUAGE DeriveGeneric     #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Main where

import           Control.Monad        (mzero)
import qualified Data.ByteString      as BS
import qualified Data.ByteString.Lazy as BL
import           Data.Text            (Text)
import qualified Data.Text            as T
import qualified Data.Text.Encoding   as T
import qualified Data.Text.Lazy       as TL
import qualified Data.Text.Lazy.Encoding as TLE
import qualified Data.Map             as M
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           Network.HTTP.Types       (Query, status200)
import           Network.HTTP.Conduit hiding (Request, queryString)
import           URI.ByteString (serializeURIRef')
import           URI.ByteString.QQ
import           Data.String.Conversions (convertString)

import           Network.OAuth.OAuth2

import           SlackKey

import           GHC.Generics
import           Data.Aeson
import           Data.Aeson.Types

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

main :: IO ()
main = do
  mgr <- newManager tlsManagerSettings
  slackKey <- getSlackKey
  print $ serializeURIRef' $ appendQueryParams [("state", state), ("scope", "client")]
        $ authorizationUrl slackKey
  putStrLn "visit the url"
  run 9988 application

state :: BS.ByteString
state = "testSlackApi"

getApiToken :: Manager -> ExchangeToken -> IO OAuth2Token
getApiToken mgr code = do
  slackKey <- getSlackKey
  let (url, body) = accessTokenUrl slackKey code
  result <- doJSONPostRequest mgr slackKey url $ body ++ [("state", state)]
  case result of
    Right token -> return token
    Left (e :: OAuth2Error Errors) -> Prelude.error $ show e

getApiCode :: Request -> ExchangeToken
getApiCode request =
  case M.lookup "code" queryMap of
    Just code -> ExchangeToken $ T.decodeUtf8 code
    Nothing   -> Prelude.error "request doesn't include code"
  where
    queryMap = convertQueryToMap $ queryString request

application :: Application
application request respond = do
    response <- handleRequest requestPath request
    respond $ responseLBS status200 [("Content-Type", "text/plain")] response
  where
    requestPath = T.intercalate "/" $ pathInfo request

handleRequest :: Text -> Request -> IO BL.ByteString
handleRequest "favicon.ico" _ = return ""
handleRequest _ request = do
  mgr <- newManager tlsManagerSettings
  token <- getApiToken mgr $ getApiCode request
  let accToken = accessToken token
  putStrLn $ convertString (atoken accToken)
  authRes <- runApiCall mgr accToken
  return $ convertString $ show authRes

runApiCall :: Manager -> AccessToken -> IO (OAuth2Result (OAuth2Error Errors) BL.ByteString)
runApiCall mgr token =
  authPostBS mgr token [uri|https://slack.com/api/auth.test|]
             [("token", convertString (atoken token))]

convertQueryToMap :: Query -> M.Map BS.ByteString BS.ByteString
convertQueryToMap query =
    M.fromList $ map normalize query
  where
    normalize (k, Just v)  = (k, v)
    normalize (k, Nothing) = (k, BS.empty)
