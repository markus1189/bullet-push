{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Network.BulletPush ( pushTo
                          , push
                          , listDevices

                          , PushType (Note, Link)
                          , noteTitle
                          , noteBody

                          , linkTitle
                          , linkUrl
                          , linkBody

                          , mkInvalidFilePush

                          , PushTarget (..)
                          , PushError (..)

                          , Token
                          , getToken
                          , mkToken
                          ) where

#if __GLASGOW_HASKELL__ < 710
import           Data.Traversable (traverse)
#endif

import           Control.Lens (over, _Left, mapped)
import           Control.Lens.Operators hiding ((.=))
import           Control.Monad.Catch (try,MonadCatch)
import           Control.Monad.IO.Class
import           Data.Aeson (toJSON, ToJSON, object, (.=))
import           Data.Aeson.Lens (_JSON, members, _String, key, values)
import           Data.Aeson.Types (Pair, Value(Object))
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString as BS
import qualified Data.HashMap.Strict as M
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Text.Encoding (decodeUtf8, encodeUtf8)
import           Network.HTTP.Client (HttpException(StatusCodeException))
import           Network.Mime (defaultMimeLookup)
import           Network.Wreq
import           System.Directory (doesFileExist)
import           System.FilePath.Lens

data PushError = PushHttpException HttpException
               | PushFileNotFoundException FilePath
               | PushFileUploadAuthorizationError HttpException
               | PushFileUploadError HttpException
               | PushInvalidDeviceIdentifier
               deriving Show

newtype Token = Token { getToken :: Text }

data PushTarget = Broadcast | Email Text | DeviceIden Text deriving Show

newtype UploadAuthorization = UploadAuthorization Value deriving Show

data PushType = Note { noteTitle :: Text
                     , noteBody :: Text
                     }
              | Link { linkTitle :: Text
                     , linkUrl :: Text
                     , linkBody :: Maybe Text
                     }
              | FilePush { filePushPath :: FilePath
                         , fileType :: Text
                         , fileUrl :: Text
                         , fileMsg :: Maybe Text
                         }
              deriving (Eq, Show)

mkInvalidFilePush :: FilePath -> Maybe Text -> PushType
mkInvalidFilePush filePath = FilePush filePath "<unknown>" "<unknown>"

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
  toJSON (FilePush name typ url body) = object [ "type" .= ("file" :: Text)
                                               , "file_name" .= name
                                               , "file_type" .= typ
                                               , "file_url" .= url
                                               , "body" .= body
                                               ]
-- TODO the special case for FilePush is awkward (creation of push
-- type in IO handled using `mkInvalidFilePush` and then calling
-- `prepareFilePush`
pushTo :: (Functor m, MonadCatch m, MonadIO m)
       => PushTarget
       -> Token
       -> PushType
       -> m (Either PushError (Response L.ByteString))
pushTo tgt token@(Token t) (FilePush file _ _ body) = do
  exists <- liftIO (doesFileExist file)
  if not exists
     then return (Left (PushFileNotFoundException file))
     else do pushE <- prepareFilePush token file body
             case pushE of
               Left e -> return (Left e)
               Right filePush ->
                 tryHttpException
                 . liftIO
                 . postWith opts ep
                 . insertTarget tgt
                 . toJSON $ filePush
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

pushTo tgt (Token t) typ = tryHttpException
                           . liftIO
                           . postWith opts ep
                           . insertTarget tgt
                           . toJSON $ typ
  where ep = "https://api.pushbullet.com/v2/pushes"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

push :: Token -> PushType -> IO (Either PushError (Response L.ByteString))
push = pushTo Broadcast

tryHttpException :: (MonadCatch m, Functor m, MonadIO m) => m a -> m (Either PushError a)
tryHttpException act = do
  r <- over (mapped . _Left) PushHttpException . try $ act
  case r of
    Left ex -> case ex of
                 PushHttpException (StatusCodeException _ hs _) ->
                   if isInvalidDeviceName hs
                   then return (Left PushInvalidDeviceIdentifier)
                   else return r
                 _ -> return r
    Right _ -> return r
  where isInvalidDeviceName =
          any (BS.isInfixOf "The param 'device_iden' has an invalid value" . snd)

insertIntoObject :: Pair -> Value -> Value
insertIntoObject (k,v) (Object o) = Object $ M.insert k v o
insertIntoObject _ v = v

insertTarget :: PushTarget -> Value -> Value
insertTarget (DeviceIden iden) v = insertIntoObject ("device_iden" .= iden) v
insertTarget Broadcast v = v
insertTarget (Email addr) v = insertIntoObject ("email" .= addr) v


prepareFilePush :: (Functor m, MonadCatch m, MonadIO m)
                => Token
                -> FilePath
                -> Maybe Text
                -> m (Either PushError PushType)
prepareFilePush token filePath maybeMsgBody = do
  eitherErrorUploadAuth <- requestUpload token (T.pack fileName)
  case eitherErrorUploadAuth of
    Left (PushHttpException e) -> return (Left (PushFileUploadAuthorizationError e))
    Left e -> return (Left e)
    Right uploadAuth -> do
      eitherErrorUrl <- performUpload filePath uploadAuth
      case eitherErrorUrl of
        Left (PushHttpException e) -> return (Left (PushFileUploadError e))
        Left e -> return (Left e)
        Right url ->
          return . Right $ FilePush { filePushPath = fileName
                                    , fileType =
                                        decodeUtf8 . defaultMimeLookup  $ T.pack fileName
                                    , fileUrl = url
                                    , fileMsg = maybeMsgBody
                                    }

  where fileName = filePath ^. filename

requestUpload :: (Functor m, MonadCatch m, MonadIO m)
              => Token
              -> Text
              -> m (Either PushError UploadAuthorization)
requestUpload (Token t) fileName = do
  let mime = decodeUtf8 $ defaultMimeLookup fileName
  res <- tryHttpException . liftIO . postWith opts ep $ toJSON $ object [ "file_name" .= fileName
                                                                        , "file_type" .= mime
                                                                        ]
  case res of
    Left e -> return $ Left e
    Right resp -> do
      let jsonContent = resp ^?! responseBody . _JSON :: Value
      return . Right . UploadAuthorization $ jsonContent
  where ep = "https://api.pushbullet.com/v2/upload-request"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)

performUpload :: (Functor m, MonadCatch m, MonadIO m)
              => FilePath
              -> UploadAuthorization
              -> m (Either PushError Text)
performUpload f (UploadAuthorization jsonBlob) = do
  res <- tryHttpException $ liftIO $ post ep $
         uploadParams ++ [partFileSource "file" f] -- Order is VERY important here
  case res of
    Left e -> return $ Left e
    Right _ -> return . Right $ jsonBlob ^?! key "file_url" . _String
  where ep = jsonBlob ^?! key "upload_url" . _String & T.unpack
        uploadParams = jsonBlob ^@.. key "data" . members . _String & over traverse (uncurry partText)

listDevices :: (Functor m, MonadCatch m, MonadIO m)
            => Token
            -> m (Either PushError [(Text, Text)])
listDevices (Token t) = tryHttpException $ do
  r <- liftIO (getWith opts ep)
  let idens = r ^.. responseBody . key "devices" . values . key "iden" . _String
      nicks = r ^.. responseBody . key "devices" . values . key "nickname" . _String
  return $ zip nicks idens
  where ep = "https://api.pushbullet.com/v2/devices"
        opts = defaults & auth ?~ oauth2Bearer (encodeUtf8 t)
