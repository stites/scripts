#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Turtle
import Data.Maybe

main :: IO ()
main = do
  (cmd, ms, mn) <- options "retry a command" parser
  retry cmd (toSeconds ms) (toRetries mn)

  where
    toSeconds :: Maybe Integer -> NominalDiffTime
    toSeconds = fromInteger . fromMaybe 15

    toRetries :: Maybe Int -> Int
    toRetries = fromMaybe 15


retry :: Text -> NominalDiffTime -> Int -> IO ()
retry cmd n = go
  where
    go :: Int -> IO ()
    go 0 = die (repr cmd <> " failed " <> repr n <> " times")
    go left =
      shell cmd empty >>= \case
        ExitSuccess   -> return ()
        ExitFailure c -> do
          print $ repr cmd <> " failed with exit code: " <> repr c
          sleep n >> go (left-1)


parser :: Parser (Text, Maybe Integer, Maybe Int)
parser = (,,)
  <$> argText "cmd" "the command to rerun in case of failure"
  <*> optional (optInteger "delay" 's' "seconds to between an attempt (default: 15s)")
  <*> optional (optInt  "retry" 'n' "number of times to retry (default: 15)")

