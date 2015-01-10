{-# LANGUAGE PartialTypeSignatures #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BulletPush ( pushTo
                          , push

                          , PushType (Note, Link, Checklist)
                          , noteTitle
                          , noteBody
                          , linkTitle
                          , linkUrl
                          , linkBody
                          , listTitle
                          , listItems

                          , mkInvalidFilePush

                          , PushTarget (..)
                          , PushError (..)

                          , Token
                          , getToken
                          , mkToken
                          ) where

import           Control.Exception (try)
import           Control.Lens (over, _Left, mapped)
import           Control.Lens.Operators hiding ((.=))
import           Data.Aeson (toJSON, ToJSON, object, (.=))
import           Data.Aeson.Lens (_JSON, members, _String, key)
import           Data.Aeson.Types (Pair, Value(Object))
import qualified Data.ByteString.Lazy as L
import qualified Data.HashMap.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8)
import           Data.Text.Encoding (encodeUtf8)
import           Network.HTTP.Client (HttpException)
import           Network.Mime (defaultMimeLookup)
import           Network.Wreq
import           System.Directory (doesFileExist)
import           System.FilePath.Lens

data PushError = PushHttpException HttpException
               | PushFileNotFoundException FilePath

newtype Token = Token { getToken :: Text }

data PushTarget = Broadcast | Email Text deriving Show

newtype UploadAuthorization = UploadAuthorization Value deriving Show

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
              | FilePush { filePushPath :: FilePath
                         , fileType :: Text
                         , fileUrl :: Text
                         , fileMsg :: Maybe Text
                         }
              deriving (Eq, Show)

mkInvalidFilePush :: FilePath -> Maybe Text -> PushType
mkInvalidFilePush filePath msg = FilePush filePath "<unknown>" "<unknown>" msg

-- Currently no validation
mkToken :: Text -> Maybe Token
mkToken = Just . Token

instance ToJSON PushType where
  toJSON (Note title body) = object [ "type" .= ("note" :: Text)
                                    , "title" .= title
                                    , "body" .= body
                                    ]
  toJSON (Link title url maybeBody) = object $ [ "type" .= ("link" :: Text)
                                               , "title" .= title
                                               , "url" .= url
                                               ] ++ maybe [] (return . ("body" .=)) maybeBody
  toJSON (Checklist title items) = object [ "type" .= ("list" :: Text)
                                          , "title" .= title
                                          , "items" .= toJSON items
                                          ]
  toJSON (FilePush name typ url body) = object [ "type" .= ("file" :: Text)
                                               , "file_name" .= name
                                               , "file_type" .= typ
                                               , "file_url" .= url
                                               , "body" .= body
                                               ]
-- TODO the special case for FilePush is awkward (creation of push
-- type in IO handled using `mkInvalidFilePush` and then calling
-- `prepareFilePush`
pushTo :: PushTarget -> Token -> PushType -> IO (Either PushError (Response L.ByteString))
pushTo tgt token@(Token t) (FilePush file _ _ body) = do
  exists <- doesFileExist file
  if not exists
     then do
       return (Left (PushFileNotFoundException file))
     else do pushE <- prepareFilePush token file body
             case pushE of
               Left e -> return (Left e)
               Right filePush ->
                 tryHttpException
                 . postWith opts ep
                 . insertTarget tgt
                 . toJSON $ filePush
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

pushTo tgt (Token t) typ = tryHttpException
                           . postWith opts ep
                           . insertTarget tgt
                           . toJSON $ typ
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

push :: Token -> PushType -> IO (Either PushError (Response L.ByteString))
push = pushTo Broadcast

tryHttpException :: IO a -> IO (Either PushError a)
tryHttpException = over (mapped . _Left) PushHttpException . try

insertIntoObject :: Pair -> Value -> Value
insertIntoObject (k,v) (Object o) = Object $ M.insert k v o
insertIntoObject _ v = v

insertTarget :: PushTarget -> Value -> Value
insertTarget Broadcast v = v
insertTarget (Email addr) v = insertIntoObject ("email" .= addr) v

prepareFilePush :: Token -> FilePath -> Maybe Text -> IO (Either PushError PushType)
prepareFilePush token filePath maybeMsgBody = do
  eitherErrorUploadAuth <- requestUpload token (T.pack fileName)
  case eitherErrorUploadAuth of
    Left e -> return (Left e)
    Right uploadAuth -> do
      eitherErrorUrl <- performUpload filePath uploadAuth
      case eitherErrorUrl of
        Left e -> return (Left e)
        Right url -> do
          return . Right $ FilePush { filePushPath = fileName
                                    , fileType =
                                        decodeUtf8 . defaultMimeLookup  $ (T.pack fileName)
                                    , fileUrl = url
                                    , fileMsg = maybeMsgBody
                                    }

  where fileName = filePath ^. filename

requestUpload :: Token -> Text -> IO (Either PushError UploadAuthorization)
requestUpload (Token t) fileName = do
  let mime = decodeUtf8 $ defaultMimeLookup fileName
  res <- tryHttpException . postWith opts ep $ toJSON $ object [ "file_name" .= fileName
                                                               , "file_type" .= mime
                                                               ]
  case res of
    Left e -> return $ Left e
    Right resp -> do
      let jsonContent = resp ^?! responseBody . _JSON :: Value
      return . Right . UploadAuthorization $ jsonContent
  where ep = "https://api.pushbullet.com/v2/upload-request"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

performUpload :: FilePath -> UploadAuthorization -> IO (Either PushError Text)
performUpload f (UploadAuthorization jsonBlob) = do
  res <- tryHttpException $ post ep $
         uploadParams ++ [partFileSource "file" f] -- Order is VERY important here
  case res of
    Left e -> return $ Left e
    Right _ -> do
      return . Right $ jsonBlob ^?! key "file_url" . _String
  where ep = jsonBlob ^?! key "upload_url" . _String & T.unpack
        uploadParams = jsonBlob ^@.. key "data" . members . _String & over traverse (uncurry partText)
