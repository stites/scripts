#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import Control.Monad.Catch


main :: IO ()
main = do
  fp <- options "toggle the `.bk` extension on a file" parser
  checkFile fp
  mv fp (moveTo fp)


parser :: Parser FilePath
parser = argPath "file" "The file to change extensions of"


moveTo :: FilePath -> FilePath
moveTo fp
  | hasExtension fp "bk" = dropExtension fp
  | otherwise            = fp <.> "bk"


checkFile :: FilePath -> IO ()
checkFile fp = do
  valid <- testfile fp
  unless valid (throwM FileDoesNotExist)


data BkExceptions = FileDoesNotExist
  deriving (Show, Exception)


