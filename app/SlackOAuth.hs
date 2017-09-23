{-# LANGUAGE DeriveGeneric       #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE QuasiQuotes         #-}
{-# LANGUAGE ScopedTypeVariables #-}

module SlackOAuth where

import           Control.Concurrent       (MVar, forkIO, newEmptyMVar, putMVar,
                                           takeMVar)
import           Control.Monad            (mzero)
import           Data.Aeson              (ToJSON, decode, encode, )
import           Data.Aeson.Types
import qualified Data.ByteString          as BS
import qualified Data.ByteString.Lazy     as BL
import qualified Data.Map                 as M
import           Data.String.Conversions  (convertString)
import           Data.Text                (Text)
import qualified Data.Text                as T
import qualified Data.Text.Encoding       as T
import qualified Data.Text.Lazy           as TL
import qualified Data.Text.Lazy.Encoding  as TLE
import           GHC.Generics
import           Network.HTTP.Conduit     hiding (Request, queryString)
import           Network.HTTP.Simple      (httpLBS, setRequestQueryString)
import           Network.HTTP.Types       (Query, status200)
import           Network.OAuth.OAuth2
import           Network.Wai
import           Network.Wai.Handler.Warp (run)
import           SlackKey
import           Types
import           URI.ByteString    (serializeURIRef')
import           URI.ByteString.QQ

data Errors =
  SomeRandomError
  deriving (Show, Eq, Generic)

instance FromJSON Errors where
  parseJSON = genericParseJSON defaultOptions { constructorTagModifier = camelTo2 '_', allNullaryToStringTag = True }

readPortFromCallbackURI :: IO Int
readPortFromCallbackURI = do
  uri <- readCallbackURI
  case readPort uri of
    Nothing   -> return 9988
    Just port -> return port

runWebServer :: IO ()
runWebServer = do
  mgr <- newManager tlsManagerSettings
  slackKey <- getSlackKey
  port <- readPortFromCallbackURI
  print $ serializeURIRef' $ appendQueryParams [("state", state), ("scope", "client")]
        $ authorizationUrl slackKey
  putStrLn "visit the url"
  shutdownMVar <- newEmptyMVar
  forkIO $ run port (application shutdownMVar)
  takeMVar shutdownMVar

state :: BS.ByteString
state = "testSlackApi"

getApiToken :: Manager -> ExchangeToken -> IO OAuth2Token
getApiToken mgr code = do
  slackKey <- getSlackKey
  let (url, body) = accessTokenUrl slackKey code
  result <- doJSONPostRequest mgr slackKey url $ body ++ [("state", state)]
  case result of
    Right token                    -> return token
    Left (e :: OAuth2Error Errors) -> Prelude.error $ show e

getApiCode :: Request -> ExchangeToken
getApiCode request =
  case M.lookup "code" queryMap of
    Just code -> ExchangeToken $ T.decodeUtf8 code
    Nothing   -> Prelude.error ("request doesn't include code: " ++ show request)
  where
    queryMap = convertQueryToMap $ queryString request

application :: MVar () -> Application
application shutdownMVar request respond = do
  response <- handleRequest shutdownMVar requestPath request
  respond $ responseLBS status200 [("Content-Type", "text/plain")] response
    where
      requestPath = T.intercalate "/" $ pathInfo request

handleRequest :: MVar () -> Text -> Request -> IO BL.ByteString
handleRequest _ "favicon.ico" _ = return ""
handleRequest _ "robots.txt" _  = return ""
handleRequest shutdownMVar _ request = do
  mgr <- newManager tlsManagerSettings
  oauthToken <- getApiToken mgr $ getApiCode request
  let token = accessToken oauthToken
  saveTokenToFile "./token.txt" token
  authRes <- runApiCall mgr token
  putMVar shutdownMVar ()
  return $ convertString $ show authRes

saveTokenToFile :: FilePath -> AccessToken -> IO ()
saveTokenToFile fp = writeFile fp . convertString . atoken

runApiCall :: Manager -> AccessToken -> IO (OAuth2Result (OAuth2Error Errors) BL.ByteString)
runApiCall mgr token =
  -- Slack needs to access token to be passed in as a parameter.
  -- Not part as the header.
  authPostBS mgr token [uri|https://slack.com/api/auth.test|]
             [("token", convertString (atoken token))]

convertQueryToMap :: Query -> M.Map BS.ByteString BS.ByteString
convertQueryToMap query =
  M.fromList $ map normalize query
    where
      normalize (k, Just v)  = (k, v)
      normalize (k, Nothing) = (k, BS.empty)

checkAuthStatus :: String -> IO Bool
checkAuthStatus token = do
  initReq <- parseRequest "https://slack.com/api/auth.test"
  let req = setRequestQueryString [("token", convertString <$> Just token)]
                                  (initReq { method = "POST" })
  response <- responseBody <$> httpLBS req
  let authTest = decode response :: Maybe AuthTest
  case authTest of
    Nothing -> return False
    Just at -> return (ok at)
