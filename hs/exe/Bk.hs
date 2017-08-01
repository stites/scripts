#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions --package text
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE ViewPatterns #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import Control.Monad.Catch
import qualified Data.Text as T


main :: IO ()
main = do
  file <- options "toggle the `.bk` extension on a file" parser
  checkFile file
  mv (rmTrail' file) (moveTo file)


parser :: Parser FilePath
parser = argPath "file" "The file to change extensions of"


moveTo :: FilePath -> FilePath
moveTo file
  | hasExtension file "bk" = dropExtension file
  | otherwise = maybe (rmTrail' file <.> "bk") fromText pathWithBk
  where
    pathWithBk :: Maybe Text
    pathWithBk = T.stripSuffix ".bk" $ rmTrail file


runOnFilePath :: (Text -> x) -> FilePath -> x
runOnFilePath go (toText->fp') = case fp' of
  Left approxPath -> go approxPath
  Right   txtPath -> go txtPath


rmTrail' :: FilePath -> FilePath
rmTrail' = fromText . rmTrail


rmTrail :: FilePath -> Text
rmTrail = runOnFilePath
  (T.dropWhileEnd (== '/'))


checkFile :: FilePath -> IO ()
checkFile file =
  testpath file >>= \valid ->
    unless valid (throwM PathDoesNotExist)


data BkExceptions = PathDoesNotExist
  deriving (Show, Exception)


