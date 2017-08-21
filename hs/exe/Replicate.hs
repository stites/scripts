#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Turtle
import Data.Maybe

main :: IO ()
main = do
  (cmd, ms, mn) <- options "replicate a command with delay" parser
  case mn of
    Nothing -> replicateCmd' cmd (toSeconds ms)
    Just n  -> replicateCmd  cmd (toSeconds ms) n
  where
    toSeconds :: Maybe Integer -> NominalDiffTime
    toSeconds = fromInteger . fromMaybe 15

replicateCmd :: Text -> NominalDiffTime -> Int -> IO ()
replicateCmd   _ _    0 = pure ()
replicateCmd cmd n left = do
  runShell cmd
  sleep n
  replicateCmd cmd n (left - 1)

replicateCmd' :: Text -> NominalDiffTime -> IO ()
replicateCmd' cmd n = do
  runShell cmd
  sleep n
  replicateCmd' cmd n

runShell :: Text -> IO ()
runShell cmd =
  shell cmd empty >>= \case
    ExitSuccess   -> pure ()
    ExitFailure c -> print $ repr cmd <> " failed with exit code: " <> repr c

parser :: Parser (Text, Maybe Integer, Maybe Int)
parser = (,,)
  <$> optText "cmd" 'c' "the command to rerun in case of failure"
  <*> optional (optInteger "delay" 'd' "seconds to between an attempt (default: 15)")
  <*> optional (optInt  "num" 'n' "number of times to replicate (default: forever)")

