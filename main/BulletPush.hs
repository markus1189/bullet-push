{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
module Main (main) where

import           Control.Exception (SomeException(..), handle)
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO,MonadIO)
import           Control.Monad.Logger
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import           Control.Retry (retrying,limitRetries,constantDelay)
import           Data.Either (isLeft)
import           Data.List (dropWhileEnd)
import           Data.Text (Text)
import qualified Data.Text as T
import           Options.Applicative
import           Prelude hiding (log)
import           System.Directory (getHomeDirectory, doesFileExist)
import           System.Exit (exitWith, ExitCode(..), exitSuccess)
import           System.FilePath ((</>))

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

log :: (MonadLogger m, MonadIO m) => Verbosity -> String -> m ()
log Normal _ = return ()
log Verbose s = liftIO . putStrLn $ "[LOG] " <> s

main :: IO ()
main = runStdoutLoggingT $ do
  cmdOpts <- liftIO (execParser cmdlineOpts)
  pbToken <- determineToken cmdOpts
  case pbToken of
    Nothing -> error "No (valid) token given and/or no (valid) token file."
    Just token -> do
      log (givenVerbosity cmdOpts) $ "Using token: " <> (T.unpack . getToken $ token)
      log (givenVerbosity cmdOpts) $ "Using push target: " <> show (pushTarget cmdOpts)
      eitherErrorResponse <- retry cmdOpts $ pushTo (pushTarget cmdOpts) token . pushType $ cmdOpts
      processResult (givenVerbosity cmdOpts) eitherErrorResponse
  where
    cmdlineOpts = info (helper <*> cmds)
                  ( fullDesc
                    <> progDesc "Push something with pushbullet."
                    <> header "bullet-push, the haskell pushbullet client" )
    retry cmdOpts = retrying (limitRetries (numRetries cmdOpts) <>
                              constantDelay (1 * 1000 * 1000))
                             (const (return . isLeft))

processResult :: (MonadLogger m, MonadIO m) => Verbosity -> Either PushError a -> m ()
processResult v (Right _) = log v "Success" >> liftIO exitSuccess
processResult v (Left e) = reportError v e

reportError :: (MonadLogger m, MonadIO m) => Verbosity -> PushError -> m ()
reportError v e = do
  log v (show e)
  liftIO (putStrLn (errorMsgFor e))
  liftIO (exitWith (ExitFailure 1))
  where errorMsgFor :: PushError -> String
        errorMsgFor (PushHttpException _) = "Error with connection, run with -v for details"
        errorMsgFor (PushFileNotFoundException f) = "Could not find file: " ++ f
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
  let v = givenVerbosity o
  case givenToken o of
    Just t -> case mkToken t of
      Just tk -> return $ Just tk
      Nothing -> do
        log v "Given token is invalid"
        return Nothing
    Nothing -> tryFromFile
  where tryFromFile :: (Applicative m, MonadIO m, MonadLogger m) => m (Maybe Token)
        tryFromFile = do
          tokenFilePath' <- tokenFilePath (tokenFile o)
          log (givenVerbosity o) $ "Trying to read token from file: " ++ tokenFilePath'
          liftIO (readTokenFile tokenFilePath')
