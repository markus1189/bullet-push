{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}
module Main (main) where

import           Control.Exception (SomeException(..), handle)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO,MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import           Control.Retry (retrying,limitRetries,constantDelay)
import qualified Data.ByteString.Char8 as B
import           Data.Char (toLower)
import           Data.List (dropWhileEnd)
import           Data.Text (Text)
import qualified Data.Text as T
import           Options.Applicative
import           System.Directory (getHomeDirectory, doesFileExist)
import           System.Exit (exitWith, ExitCode(..), exitSuccess)
import           System.FilePath ((</>))
import           System.Log.FastLogger (fromLogStr)

import           Network.BulletPush

defaultTokenFile :: FilePath
defaultTokenFile = ".bulletpush"

defaultTokenFilePath :: (Applicative m, MonadIO m) => m FilePath
defaultTokenFilePath = (</>) <$> liftIO getHomeDirectory <*> pure defaultTokenFile

data Verbosity = Normal | Verbose | Quiet | Debug
data PushToken = TokenFile FilePath | TokenString Text

data CmdlineOpts = CmdlineOpts { givenVerbosity :: Verbosity
                               , pushTarget :: PushTarget
                               , pushToken :: PushToken
                               , numRetries :: Int
                               , pushAction :: Action
                               }


logger :: Loc -> LogSource -> LogLevel -> LogStr -> IO ()
logger loc src LevelDebug msg =
  B.putStr . fromLogStr $ defaultLogStr loc src LevelDebug msg
logger _ _ level msg = B.putStrLn (lvl <> fromLogStr msg)
  where lvl = case level of
                LevelInfo -> ""
                _ -> B.pack ("[" ++ map toLower (drop 5 (show level)) ++ "] ")

main :: IO ()
main = do
  defTokenPath <- defaultTokenFilePath
  cmdOpts <- liftIO (execParser (cmdlineOpts defTokenPath))
  flip runLoggingT logger . filterLogger (logFilter (givenVerbosity cmdOpts)) $ do
    pbToken <- determineToken cmdOpts
    case pbToken of
      Nothing -> do
        $logError "No (valid) token given and/or no (valid) token file."
        liftIO (exitWith (ExitFailure 1))
      Just token ->
        case pushAction cmdOpts of
          NewPush typ -> do
            $logDebug $ "Using push target: " <> T.pack (show (pushTarget cmdOpts))
            eitherErrorResponse <- retry cmdOpts
                                 . pushTo (pushTarget cmdOpts) token
                                 $ typ

            processResult eitherErrorResponse
          ListDevices -> do
            eitherDevs <- retry cmdOpts (listDevices token)
            case eitherDevs of
              Left e -> reportError e
              Right devs ->  do
                let nicks = map (\(nick,iden) -> ("- " <> nick <> ": " <> iden))
                                devs
                $logInfo (T.intercalate "\n" ("Available devices: " : nicks))
  where
    cmdlineOpts defTokenPath = info (helper <*> cmds defTokenPath)
                               ( fullDesc
                                 <> progDesc "Push something with pushbullet."
                                 <> header "bullet-push - the Pushbullet client" )
    retry cmdOpts =
      retrying (limitRetries (numRetries cmdOpts) <>
                constantDelay (1 * 1000 * 1000))
               (\n r ->
                  case r of
                    Right _ -> return False
                    Left (PushFileNotFoundException _) -> return False
                    Left PushInvalidDeviceIdentifier -> return False
                    Left err -> do
                      $logError ("Retry " <> T.pack (show (n + 1)) <> "/" <>
                                 T.pack (show (numRetries cmdOpts)) <> ": " <>
                                 errorMsgFor err)
                      return True)
    logFilter Verbose _ lvl = lvl > LevelDebug
    logFilter Normal _ lvl = lvl == LevelInfo
    logFilter Quiet _ _ = False
    logFilter Debug _ _ = True

processResult :: (MonadLogger m, MonadIO m) => Either PushError a -> m ()
processResult (Right _) = $logInfo "Success" >> liftIO exitSuccess
processResult (Left e) = reportError e

reportError :: (MonadLogger m, MonadIO m) => PushError -> m ()
reportError e = do
  $logDebug (T.pack (show e))
  $logError (errorMsgFor e)
  $logInfo "Unable to push."
  liftIO (exitWith (ExitFailure 1))

errorMsgFor :: PushError -> Text
errorMsgFor (PushHttpException e) =
  "Error with connection: " <> T.pack (show e)
errorMsgFor (PushFileNotFoundException f) =
  "Could not find file: " <> T.pack f
errorMsgFor (PushFileUploadAuthorizationError _) =
  "Error requesting file upload authorization"
errorMsgFor (PushFileUploadError _) =
  "Error during file upload"
errorMsgFor PushInvalidDeviceIdentifier =
  "Invalid device identifier"

data Action = NewPush PushType | ListDevices

cmds :: FilePath -> Parser CmdlineOpts
cmds defTokenPath =
  CmdlineOpts <$> verbosityOpt
              <*> targetOpt
              <*> tokenOpt defTokenPath
              <*> retriesOpt
              <*> subparser (file <> link <> note <> device)
  where file = command "file" (info (NewPush <$> fileParser) (progDesc "Push a file"))
        link = command "link"(info (NewPush <$> linkParser) (progDesc "Push a link"))
        note = command "note"(info (NewPush <$> noteParser) (progDesc "Push a note"))
        device = command "devices"(info (pure ListDevices) (progDesc "List your devices"))

tokenOpt :: String -> Parser PushToken
tokenOpt defTokenPath =
      TokenString . T.pack <$> strOption (long "token" <>
                                          metavar "TOKEN" <>
                                          help "Use TOKEN for authentication")
  <|> TokenFile <$> strOption (long "token-file" <>
                               metavar "FILE" <>
                               help "Read authentication token from FILE" <>
                               value defTokenPath <>
                               showDefault)

verbosityOpt :: Parser Verbosity
verbosityOpt = flag' Verbose
                     (long "verbose" <> short 'v' <> help "Enable verbose mode")
           <|> flag' Quiet
                     (long "quiet" <> short 'q' <> help "Don't print output")
           <|> flag' Debug
                     (long "debug" <> help "Print debugging output")
           <|> pure Normal

targetOpt :: Parser PushTarget
targetOpt = Email . T.pack
                  <$> strOption (long "email" <>
                                 short 'e' <>
                                 metavar "EMAIL" <>
                                 help "Send push to EMAIL")
                  <|> DeviceIden . T.pack
                  <$> strOption (long "iden" <>
                                 short 'i' <>
                                 metavar "IDEN" <>
                                 help "Send push to device with identifier IDEN")
                  <|> pure Broadcast

retriesOpt :: Parser Int
retriesOpt = option auto
                    (long "retries" <>
                     short 'r' <>
                     help "Number of retries before giving up" <>
                     value 3 <>
                     showDefault)

noteParser :: Parser PushType
noteParser = Note
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> (T.pack <$> argument str (metavar "BODY"))

linkParser :: Parser PushType
linkParser = Link
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> (T.pack <$> argument str (metavar "URL"))
         <*> optional (T.pack <$> argument str (metavar "BODY"))

fileParser :: Parser PushType
fileParser = mkInvalidFilePush
         <$> argument str (metavar "FILE")
         <*> optional (T.pack <$> argument str (metavar "BODY"))

readTokenFile :: FilePath -> IO (Maybe Token)
readTokenFile path = runMaybeT $ do
  contents <- MaybeT $ maybeReadFile path
  let ls = lines contents
  guard . not . null $ ls
  MaybeT (return . mkToken . T.pack . dropWhileEnd (== '\r') . head $ ls)

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile path = handle (\(SomeException _) -> pure Nothing) . runMaybeT $ do
  exists <- liftIO $ doesFileExist path
  guard exists
  liftIO $ readFile path

determineToken :: (Applicative m, MonadLogger m, MonadIO m)
               => CmdlineOpts
               -> m (Maybe Token)
determineToken o =
  case pushToken o of
    TokenString t -> case mkToken t of
                       Just tk -> return $ Just tk
                       Nothing -> do
                         $logError "Given token is invalid"
                         return Nothing
    TokenFile file -> do
      $logDebug $ "Trying to read token from file: " <> T.pack file
      liftIO (readTokenFile file)
