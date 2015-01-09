{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BulletPush (push, PushType(..)) where

import           Control.Exception (try)
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L
import           Data.Text (Text)
import           Network.HTTP.Client (HttpException)
import           Network.Wreq
import           Network.Wreq.Types (Postable(..))

data PushType = Note { noteTitle :: Text
                     , noteBody :: Text
                     }
              | Link { linkTitle :: Text
                     , linkUrl :: Text
                     , linkBody :: Maybe Text
                     }
              deriving (Eq, Show)

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

push :: Options -> PushType -> IO (Either HttpException (Response L.ByteString))
push opts = liftIO . tryHttpException . postWith opts ep
  where ep = "https://api.pushbullet.com/v2/pushes"

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try
