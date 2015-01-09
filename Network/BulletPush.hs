{-# LANGUAGE OverloadedStrings #-}
module Network.BulletPush ( pushTo
                          , push

                          , PushType (..)
                          , PushTarget (..)

                          , Token
                          , getToken
                          , mkToken
                          ) where

import           Control.Exception (try)
import           Control.Lens.Operators hiding ((.=))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (toJSON, ToJSON, object, (.=))
import           Data.Aeson.Types (Pair, Value(Object))
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client (HttpException)
import           Network.Wreq
import qualified Data.HashMap.Strict as M

newtype Token = Token { getToken :: Text }

data PushTarget = Broadcast | Email Text deriving Show

data PushType = Note { noteTitle :: Text
                     , noteBody :: Text
                     }
              | Link { linkTitle :: Text
                     , linkUrl :: Text
                     , linkBody :: Maybe Text
                     }
              | Checklist { listTitle :: Text
                          , listItems :: [Text]
                          }
              deriving (Eq, Show)

-- Currently no validation
mkToken :: Text -> Maybe Token
mkToken = Just . Token

instance ToJSON PushType where
  toJSON (Note title body) = object [ "type" .= ("note" :: Text)
                                    , "title" .= title
                                    , "body" .= body]
  toJSON (Link title url maybeBody) = object $ [ "type" .= ("link" :: Text)
                                               , "title" .= title
                                               , "url" .= url
                                               ] ++ maybe [] (return . ("body" .=)) maybeBody
  toJSON (Checklist title items) = object [ "type" .= ("list" :: Text)
                                          , "title" .= title
                                          , "items" .= toJSON items
                                          ]

pushTo :: PushTarget -> Token -> PushType -> IO (Either HttpException (Response L.ByteString))
pushTo tgt (Token t) = liftIO . tryHttpException . postWith opts ep . insertTarget tgt . toJSON
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

push :: Token -> PushType -> IO (Either HttpException (Response L.ByteString))
push = pushTo Broadcast

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try

insertIntoObject :: Pair -> Value -> Value
insertIntoObject (k,v) (Object o) = Object $ M.insert k v o
insertIntoObject _ v = v

insertTarget :: PushTarget -> Value -> Value
insertTarget Broadcast v = v
insertTarget (Email addr) v = insertIntoObject ("email" .= addr) v
