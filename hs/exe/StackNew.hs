#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T
import Control.Monad.Catch

pathToDefaultTemplate :: Text
pathToDefaultTemplate = "~/git/stack-templates/skeleton"

main :: IO ()
main = do
  name <- options "toggle the `.bk` extension on a file" parser
  sh . fromString . T.unpack $ "stack new " <> name <> " --bare " <> pathToDefaultTemplate


parser :: Parser Text
parser = argText "name" "The name of the project to create"


