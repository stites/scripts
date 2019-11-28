{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE NamedFieldPuns #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (FilePath)
import Text.HTML.TagSoup as TS
import Turtle hiding (guard, ls, fp, empty)
import Control.Monad hiding (guard)
import Control.Exception.Safe
import Control.Effect.Error
import Control.Effect.Empty
import Control.Carrier.Empty.Maybe
import Control.Carrier.Error.Either
import Control.Lens
import Data.List.NonEmpty (NonEmpty((:|)))
import qualified Data.List.NonEmpty as NE
import qualified Filesystem.Path.CurrentOS as FS
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import qualified Data.Text.Read as TR
import qualified Data.Text.IO as TIO
import Network.Wreq (responseBody, responseStatus, statusCode, redirects, defaults, getWith)
import qualified Streaming as S
import qualified Streaming.Prelude as S
import qualified Data.ByteString.Lazy as BS

main :: IO ()
main = do
  file <- options "convert a pocket export to a todoist project import file" parser

  runError (do
    ls <- taglines . TS.parseTags <$> getFile file
    -- liftIO $ deleteFileLock file
    ix <- liftIO $ readOrCreateFileLock file
    liftIO $ print ix
    sequence $ lineitem <$> drop ix ls
    ) >>= \case
      Left txterr -> throwText txterr
      Right xs -> S.print . S.mapM (go file) . S.take 3 $ S.each xs
  where
    parser :: Parser FilePath
    parser = argPath "file" "HTML export file from pocket"

    go :: FilePath -> LinkItem -> IO LinkItem
    go fp li = runError (updateTitle fp li) >>= \case
        Left err -> throwText err
        Right li' -> pure li'

getFile :: MonadIO io => Has (Throw Text) sig io => FilePath -> io Text
getFile file =
  liftIO (testpath file) >>= \case
    False -> throwError $ "path " <> T.pack fp <> " is invalid"
    True -> liftIO . TIO.readFile $ fp
  where
    fp = FS.encodeString file

readFileLock :: MonadIO io => Has Empty sig io => FilePath -> io Int
readFileLock fp = do
  liftIO (testpath (toLockPath fp)) >>= guard
  liftIO (TR.double <$$> TIO.readFile $ toLockStr fp)
  >>= \case
    Left _ -> empty
    Right (i, _) -> pure $ truncate i
  where
    (<$$>) :: Functor f => Functor g => (a -> b) -> f (g a) -> f (g b)
    (<$$>) = fmap.fmap

writeFileLock :: MonadIO io => FilePath -> Int -> io ()
writeFileLock fp i = liftIO $ TIO.writeFile (toLockStr fp) (T.pack $ show i)

incFileLock :: MonadIO io => FilePath -> io ()
incFileLock filepath = liftIO (runEmpty (readFileLock filepath))
  >>= \case
    Nothing -> writeFileLock filepath 1
    Just i -> writeFileLock filepath (i+1)

deleteFileLock :: MonadIO io => FilePath -> io ()
deleteFileLock filepath = liftIO $ rm (toLockPath filepath)

readOrCreateFileLock :: MonadIO io => FilePath -> io Int
readOrCreateFileLock filepath =
  liftIO (runEmpty (readFileLock filepath)) >>= \case
    Nothing -> writeFileLock filepath 0 >> pure 0
    Just i -> pure i

toLockPath :: FilePath -> FilePath
toLockPath = (`FS.addExtension` "lock")

toLockStr :: FilePath -> String
toLockStr = FS.encodeString . toLockPath

data LinkItem
  = LinkItem
  { tags :: [Text]
  , link :: Text
  , title :: Text
  } deriving (Eq, Show)

taglines :: [Tag Text] -> [NonEmpty (Tag Text)]
taglines = fmap NE.fromList . TS.partitions (isTagOpenName "a")

lineitem :: Has (Throw Text) sig m => NonEmpty (Tag Text) -> m LinkItem
lineitem (atag:|tags) = do
  when (T.null title) $ throwError ("malformed html: title is empty" :: Text)
  pure $ LinkItem
    { tags = T.splitOn "," $ fromAttrib "tags" atag
    , link = fromAttrib "href" atag
    , title = title
    }
  where
    title :: Text
    title = T.filter (/= '\n') $ innerText tags

isURL :: Text -> Bool
isURL t = (any (`T.isPrefixOf` t) ["https://", "http://"])

isPDF :: Text -> Bool
isPDF t = ".pdf" `T.isSuffixOf` t

updateTitle :: MonadIO io => Has (Throw Text) sig io => FilePath -> LinkItem -> io LinkItem
updateTitle fp i@LinkItem{title, link} = liftIO $ do
  ret <-
    if not (isURL title)
    then pure i
    else liftIO $ do
      r <- getWith (defaults & redirects .~ 5) (T.unpack link)
      if r ^. responseStatus . statusCode >= 200 && r ^. responseStatus . statusCode < 300
      then case (partitions (\a -> isTagOpenName "title" a || isTagCloseName "title" a) $ parseTags (r ^. responseBody)) of
          [] -> pure i
          (a:_) -> pure $ i { title = TE.decodeUtf8 (BS.toStrict $ innerText a) }
      else pure i
  incFileLock fp
  pure ret

throwText :: Text -> IO a
throwText = throwString . T.unpack


