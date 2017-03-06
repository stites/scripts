#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import Control.Monad.Catch
import Data.Maybe

main :: IO ()
main = do
  (cmd, ms, mn) <- options "retry a command" parser
  go cmd (fromMaybe 15 ms) $ NominalDiffTime (fromMaybe 15 mn)

  where
    go :: Text -> NominalDiffTime -> Int -> IO ()
    go cmd n = go'
      where
        go' :: Int -> IO ()
        go' 0 = die (repr cmd <> " failed " <> repr n <> " times")
        go' s =
          shell cmd empty >>= \case
            ExitSuccess   -> return ()
            ExitFailure c -> do
              print $ repr cmd <> " failed with exit code: " <> repr c
              sleep n
              go' (s-1)


parser :: Parser (Text, Maybe Double, Maybe Int)
parser = (,,)
  <$> argText "cmd" "the command to rerun in case of failure"
  <*> optional (optDouble "delay" 's' "seconds to between an attempt (default: 15s)")
  <*> optional (optInt  "retry" 'n' "number of times to retry (default: 15)")

