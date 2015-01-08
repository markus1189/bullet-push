{-# LANGUAGE OverloadedStrings, FlexibleContexts #-}

import           Control.Applicative ((<*>), (<$>), pure)
import           Control.Exception (try)
import           Control.Lens.Operators
import           Control.Monad.IO.Class (liftIO)
import qualified Data.ByteString.Lazy as L
import qualified Data.ByteString.Char8 as BC
import           Data.List (dropWhileEnd)
import           Data.Monoid ((<>))
import           Data.Text (Text)
import qualified Data.Text as T
import           Network.HTTP.Client (HttpException)
import           Network.Wreq
import           Network.Wreq.Types (Postable(..))
import qualified Options.Applicative as O
import           System.Directory (getHomeDirectory, doesFileExist)
import           System.FilePath ((</>))

data PushType = Note Text Text
              | Link Text Text (Maybe Text)
              deriving (Show)

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


tokenFile :: FilePath
tokenFile = ".bulletpush"

main :: IO ()
main = do
  tokenFilePath <- (</>) <$> getHomeDirectory <*> pure tokenFile
  dotFileExists <- doesFileExist tokenFilePath
  token <- if dotFileExists
              then BC.pack . dropWhileEnd (`elem` ("\r\n" :: String)) <$> readFile tokenFilePath
              else error "'~/.bulletpush' not found"
  eitherResponse <- push (opts token) =<< O.execParser cmdlineOpts
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

tryHttpException :: IO a -> IO (Either HttpException a)
tryHttpException = try

push :: Options -> PushType -> IO (Either HttpException (Response L.ByteString))
push opts = liftIO . tryHttpException . postWith opts ep
  where ep = "https://api.pushbullet.com/v2/pushes"
