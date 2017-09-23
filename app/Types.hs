{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE QuasiQuotes #-}

module Types(
  Bytes
  , Text
  , JPEGMonad
  , ExceptT
  , Brain(..)
  , World(..)
  , Speech(..)
  , Speech'(..)
  , api
  , bearer
  , wss
  , id
  , u
  , Line(..)
  , channel
  , truth
  , withInbox
  , AuthTest(..)
) where

import           BasePrelude           hiding (lazy, putStrLn, (&))
import           Control.Lens          hiding ((.=))
import           Control.Monad.Except  (ExceptT (..))
import           Data.Aeson
import           Data.Aeson.Types
import           Data.ByteString       (ByteString)
import           Data.Text             (Text)
import           Data.Text.Strict.Lens
import           URI.ByteString

type JPEGMonad = ExceptT String IO
type Bytes = ByteString

data Brain = Brain { _api, _bearer :: !Text } deriving (Show)
newtype World = World { _wss :: URI } deriving (Show)
data Speech = Speech { _replyTo :: !Line, _t :: !Text } deriving (Show)
data Speech' = Speech' { _speech :: !Speech, _id :: !Int } deriving (Show)
data Line = Line { _channel :: !Text, _user :: !Text, _truth :: !Text } deriving (Eq, Ord, Show)

makeLenses ''Line
makeLenses ''Brain
makeLenses ''World

u :: String -> String
u = ("https://slack.com" ++)

instance FromJSON World where
  parseJSON (Object o) = do
    ok <- o .: "ok"
    url <- o .: "url"
    guard ok
    either (fail . show) (return . World) (parseURI strictURIParserOptions (url ^. re utf8))

  parseJSON invalid =
    typeMismatch "World" invalid

instance ToJSON Speech' where
  toJSON (Speech' (Speech line t) id_) =
    object [ "id" .= id_
           , "channel" .= (line ^. channel)
           , "text" .= t
           , "type" .= ("message" :: String)
           ]
  -- toEncoding (Speech' (Speech channel t) id_) =
  --   pairs ("id" .= id_ <> "channel" .= channel <> "text" .= t <> "type" .= ("message" :: String))

instance FromJSON Line where
  parseJSON (Object o) = do
    reply_to <- o .:? "reply_to"
    guard (not (isReply reply_to))
    Line <$> o .: "channel" <*> o .: "user" <*> o .: "text"
    where
      isReply :: Maybe Int -> Bool
      isReply = isJust

  parseJSON invalid =
    typeMismatch "Line" invalid

withInbox :: FromJSON a => Chan Bytes -> (a -> IO b) -> IO ()
withInbox inbox cont = do
  chan <- dupChan inbox
  (void . forkIO . forever) $ do
    bytes <- readChan chan
    case eitherDecode (bytes ^. lazy) of
      Left _ ->
        return ()
      Right o ->
        void (cont o)

data AuthTest = AuthTest { ok :: Bool, url :: !Text, team :: !Text, authuser :: !Text,
                           teamId :: !Text, userId :: !Text } deriving (Show)

instance FromJSON AuthTest where
  parseJSON (Object o) =
    AuthTest <$> o .: "ok"
             <*> o .: "url"
             <*> o .: "team"
             <*> o .: "user"
             <*> o .: "team_id"
             <*> o .: "user_id"
  parseJSON _ = mzero
