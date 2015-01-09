{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<*>), (<$>), pure)
import           Data.Monoid ((<>))
#endif

import           Control.Exception (SomeException(..), handle)
import           Control.Lens.Operators
import           Control.Monad (guard)
import           Control.Monad.IO.Class (liftIO)
import           Control.Monad.Trans.Maybe (runMaybeT, MaybeT(..))
import           Data.List (dropWhileEnd)
import           Data.Monoid (First(..))
import           Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as T
import           Network.Wreq hiding (header)
import           Options.Applicative
import           Prelude hiding (log)
import           System.Directory (getHomeDirectory, doesFileExist)
import           System.FilePath ((</>))


import           Network.BulletPush (push, PushType(..))

defaultTokenFile :: FilePath
defaultTokenFile = ".bulletpush"

tokenFilePath :: FilePath -> IO FilePath
tokenFilePath path = (</>) <$> getHomeDirectory <*> pure path

data Verbosity = Normal | Verbose

data CmdlineOpts = CmdlineOpts { givenVerbosity :: Verbosity
                               , givenToken :: Maybe Text
                               , tokenFile :: FilePath
                               , pushType :: PushType
                               }

log :: Verbosity -> String -> IO ()
log Normal _ = return ()
log Verbose s = putStrLn s

main :: IO ()
main = do
  cmdOpts <- execParser cmdlineOpts
  tokenFilePath' <- tokenFilePath (tokenFile cmdOpts)
  pbTokenFile <- readTokenFile tokenFilePath'
  let pbToken = getFirst $ foldMap First [givenToken cmdOpts, pbTokenFile]
  case pbToken of
    Nothing -> error "No token given and no ~/.bulletpush file."
    Just token -> do
      log (givenVerbosity cmdOpts) $ "Using token: " ++ (T.unpack token)
      eitherResponse <- push (opts token) . pushType $ cmdOpts
      case eitherResponse of
        Left e -> log (givenVerbosity cmdOpts) (show e) >> putStrLn "Failed."
        Right _ -> putStrLn "Success."
  where
    cmdlineOpts = info (helper <*> cmds)
                  ( fullDesc
                    <> progDesc "Push something with pushbullet."
                    <> header "Haskell pushbullet client" )
    opts t = defaults & auth ?~ oauth2Bearer (T.encodeUtf8 t)

cmds :: Parser CmdlineOpts
cmds = CmdlineOpts <$> verbosity
                   <*> optional (T.pack <$> tokenOpt)
                   <*> tokenFileOpt
                   <*> subparser (command "note" (info noteParser (progDesc "Push a note"))
                   <> command "link" (info linkParser (progDesc "Push a link")))
  where tokenOpt =
          strOption (long "token"
                  <> metavar "TOKEN"
                  <> help "Use TOKEN for authentication")
        tokenFileOpt =
          strOption (long "token-file"
                  <> metavar "FILE"
                  <> help ("Read authentication token from FILE, defaults to: ~/" <> defaultTokenFile)
                  <> value defaultTokenFile)
        verbosity = flag Normal Verbose (long "verbose" <> short 'v' <> help "Enable verbose mode")

noteParser :: Parser PushType
noteParser = Note
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> (T.pack <$> argument str (metavar "BODY"))

linkParser :: Parser PushType
linkParser = Link
         <$> (T.pack <$> argument str (metavar "TITLE"))
         <*> (T.pack <$> argument str (metavar "URL"))
         <*> optional (T.pack <$> argument str (metavar "BODY"))

readTokenFile :: FilePath -> IO (Maybe Text)
readTokenFile path = runMaybeT $ do
  contents <- MaybeT $ maybeReadFile path
  let ls = lines contents
  guard . not . null $ ls
  return . T.pack . dropWhileEnd (== '\r') . head $ ls

maybeReadFile :: FilePath -> IO (Maybe String)
maybeReadFile path = handle (\(SomeException _) -> pure Nothing) . runMaybeT $ do
  exists <- liftIO $ doesFileExist path
  guard exists
  liftIO $ readFile path
