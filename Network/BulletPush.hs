{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BulletPush (push, PushType(..), Token, mkToken, getToken) where

import           Control.Exception (try)
import           Control.Lens.Operators hiding ((.=))
import           Control.Monad.IO.Class (liftIO)
import           Data.Aeson (toJSON, ToJSON, object, (.=))
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client (HttpException)
import           Network.Wreq

newtype Token = Token { getToken :: Text }

data PushType = Note { noteTitle :: Text
                     , noteBody :: Text
                     }
              | Link { linkTitle :: Text
                     , linkUrl :: Text
                     , linkBody :: Maybe Text
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

push :: Token -> PushType -> IO (Either HttpException (Response L.ByteString))
push (Token t) = liftIO . tryHttpException . postWith opts ep . toJSON
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try
