{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

#if __GLASGOW_HASKELL__ < 710
import           Control.Applicative ((<*>), (<$>), pure)
#else
import           Control.Applicative ((<$>))
#endif

import           Control.Lens.Operators
import qualified Data.ByteString.Char8 as BC
import           Data.List (dropWhileEnd)
import           Data.Monoid ((<>))
import qualified Data.Text as T
import           Network.Wreq
import qualified Options.Applicative as O
import           System.Directory (getHomeDirectory, doesFileExist)
import           System.FilePath ((</>))

import Network.BulletPush (push, PushType(..))

tokenFile :: FilePath
tokenFile = ".bulletpush"

main :: IO ()
main = do
  tokenFilePath <- (</>) <$> getHomeDirectory <*> pure tokenFile
  dotFileExists <- doesFileExist tokenFilePath
  token <- if dotFileExists
              then BC.pack . dropWhileEnd (`elem` ['\r','\n']) <$> readFile tokenFilePath
              else error "'~/.bulletpush' not found, exiting."
  eitherResponse <- O.execParser cmdlineOpts >>= push (opts token)
  case eitherResponse of
    Left _ -> putStrLn "Failed."
    Right _ -> putStrLn "Success."
  where
    cmdlineOpts = O.info (O.helper <*> cmds)
                  ( O.fullDesc
                    <> O.progDesc "Push something with pushbullet."
                    <> O.header "Haskell pushbullet client" )
    opts t = defaults & auth ?~ oauth2Bearer t

cmds :: O.Parser PushType
cmds = O.subparser ( O.command "note" (O.info noteParser (O.progDesc "Push a note"))
                    <> O.command "link" (O.info linkParser (O.progDesc "Push a link")))

noteParser :: O.Parser PushType
noteParser = Note
         <$> (T.pack <$> O.argument O.str (O.metavar "TITLE"))
         <*> (T.pack <$> O.argument O.str (O.metavar "BODY"))

linkParser :: O.Parser PushType
linkParser = Link
         <$> (T.pack <$> O.argument O.str (O.metavar "TITLE"))
         <*> (T.pack <$> O.argument O.str (O.metavar "URL"))
         <*> O.optional (T.pack <$> O.argument O.str (O.metavar "BODY"))
