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

tokenFilePath :: (Applicative m, MonadIO m) => FilePath -> m FilePath
tokenFilePath path = (</>) <$> liftIO getHomeDirectory <*> pure path

data Verbosity = Normal | Verbose

data CmdlineOpts = CmdlineOpts { givenVerbosity :: Verbosity
                               , pushTarget :: PushTarget
                               , givenToken :: Maybe Text
                               , tokenFile :: FilePath
                               , pushType :: PushType
                               , numRetries :: Int
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
  cmdOpts <- liftIO (execParser cmdlineOpts)
  flip runLoggingT logger . filterLogger (logFilter (givenVerbosity cmdOpts)) $ do
    pbToken <- determineToken cmdOpts
    case pbToken of
      Nothing -> error "No (valid) token given and/or no (valid) token file."
      Just token -> do
        $logDebug $ "Using token: " <> getToken token
        $logDebug $ "Using push target: " <> T.pack (show (pushTarget cmdOpts))
        eitherErrorResponse <- retry cmdOpts $ pushTo (pushTarget cmdOpts) token . pushType $ cmdOpts
        processResult eitherErrorResponse
  where
    cmdlineOpts = info (helper <*> cmds)
                  ( fullDesc
                    <> progDesc "Push something with pushbullet."
                    <> header "bullet-push - the Haskell pushbullet client" )
    retry cmdOpts = retrying (limitRetries (numRetries cmdOpts) <>
                              constantDelay (1 * 1000 * 1000))
                             (const (\r -> case r of
                                             Right _ -> return False
                                             Left err -> do
                                               $logError (errorMsgFor err)
                                               return True))
    logFilter Verbose _ _ = True
    logFilter Normal _ lvl = lvl == LevelInfo

processResult :: (MonadLogger m, MonadIO m) => Either PushError a -> m ()
processResult (Right _) = $logInfo "Success" >> liftIO exitSuccess
processResult (Left e) = reportError e

reportError :: (MonadLogger m, MonadIO m) => PushError -> m ()
reportError e = do
  $logDebug (T.pack (show e))
  liftIO (putStrLn "Unable to push.")
  liftIO (exitWith (ExitFailure 1))

errorMsgFor :: PushError -> Text
errorMsgFor (PushHttpException e) = "Error with connection: " <> T.pack (show e)
errorMsgFor (PushFileNotFoundException f) = "Could not find file: " <> T.pack f
errorMsgFor (PushFileUploadAuthorizationError _) = "Error requesting file upload authorization"
errorMsgFor (PushFileUploadError _) = "Error during file upload"

cmds :: Parser CmdlineOpts
cmds = CmdlineOpts <$> verbosity
                   <*> targetOpt
                   <*> optional (T.pack <$> tokenOpt)
                   <*> tokenFileOpt
                   <*> subparser (command "address" (info addressParser (progDesc "Push an address"))
                               <> command "file" (info fileParser (progDesc "Push a file"))
                               <> command "link" (info linkParser (progDesc "Push a link"))
                               <> command "list" (info listParser (progDesc "Push a checklist"))
                               <> command "note" (info noteParser (progDesc "Push a note")))
                   <*> option auto (long "retries" <> short 'r' <> help "Number of retries before giving up" <> value 2 <> showDefault)
  where tokenOpt =
          strOption (long "token"
                  <> metavar "TOKEN"
                  <> help "Use TOKEN for authentication")
        tokenFileOpt =
          strOption (long "token-file"
                  <> metavar "FILE"
                  <> help ("Read authentication token from FILE")
                  <> value defaultTokenFile
                  <> showDefaultWith ("~/"<>))
        verbosity = flag Normal Verbose (long "verbose" <> short 'v' <> help "Enable verbose mode")
        targetOpt = Email . T.pack <$> strOption (long "email" <> short 'e' <> metavar "EMAIL" <> help "Send push to EMAIL")
                <|> pure Broadcast

noteParser :: Parser PushType
noteParser = Note
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> (T.pack <$> argument str (metavar "BODY"))

linkParser :: Parser PushType
linkParser = Link
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> (T.pack <$> argument str (metavar "URL"))
         <*> optional (T.pack <$> argument str (metavar "BODY"))

listParser :: Parser PushType
listParser = Checklist
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> many (T.pack <$> argument str (metavar "ITEM"))

fileParser :: Parser PushType
fileParser = mkInvalidFilePush
         <$> argument str (metavar "FILE")
         <*> optional (T.pack <$> argument str (metavar "BODY"))

addressParser :: Parser PushType
addressParser = Address
            <$> (T.pack <$> argument str (metavar "NAME"))
            <*> (T.pack <$> argument str (metavar "ADDRESS"))

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
determineToken o = do
  case givenToken o of
    Just t -> case mkToken t of
      Just tk -> return $ Just tk
      Nothing -> do
        $logError "Given token is invalid"
        return Nothing
    Nothing -> tryFromFile
  where tryFromFile :: (Applicative m, MonadIO m, MonadLogger m) => m (Maybe Token)
        tryFromFile = do
          tokenFilePath' <- tokenFilePath (tokenFile o)
          $logDebug $ "Trying to read token from file: " <> T.pack tokenFilePath'
          liftIO (readTokenFile tokenFilePath')
