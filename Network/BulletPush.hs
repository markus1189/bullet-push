{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BulletPush (push, PushType(..), Token, mkToken, getToken) where

import           Control.Exception (try)
import           Control.Lens.Operators
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client (HttpException)
import           Network.Wreq
import           Network.Wreq.Types (Postable(..))

newtype Token = Token {getToken :: Text}

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

instance Postable PushType where
  postPayload (Note title body) = postPayload [ "type" := ("note" :: Text)
                                              , "title" := title
                                              , "body" := body
                                              ]

  postPayload (Link title url maybeBody) = postPayload [ "type" := ("link" :: Text)
                                                       , "title" := title
                                                       , "body" := maybeBody
                                                       , "url" := url
                                                       ]

push :: Token -> PushType -> IO (Either HttpException (Response L.ByteString))
push (Token t) = liftIO . tryHttpException . postWith opts ep
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try
