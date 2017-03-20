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
  file <- options "toggle the `.bk` extension on a file" parser
  checkFile file
  mv file (moveTo file)


parser :: Parser FilePath
parser = argPath "file" "The file to change extensions of"


moveTo :: FilePath -> FilePath
moveTo file
  | hasExtension file "bk" = dropExtension file
  | otherwise              = file <.> "bk"


checkFile :: FilePath -> IO ()
checkFile file = do
  valid <- testfile file
  unless valid (throwM FileDoesNotExist)


data BkExceptions = FileDoesNotExist
  deriving (Show, Exception)


