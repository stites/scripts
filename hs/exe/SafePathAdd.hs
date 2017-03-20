#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}
{-# LANGUAGE DeriveAnyClass #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T
import Control.Monad.Catch
import Control.Arrow


main :: IO ()
main = do
  (dir, placement) <- (id *** placementFromBool) <$> options "safely add an existing directory to $PATH" parser
  Just envPath <- need "PATH"
  safePathAdd dir placement envPath


safePathAdd :: (MonadThrow io, MonadIO io) => FilePath -> Placement -> Text -> io ()
safePathAdd dir place path =
  getSafePathAdd dir place path
  >>= maybe (return ()) (export "PATH")


getSafePathAdd :: (MonadThrow io, MonadIO io) => FilePath -> Placement -> Text -> io (Maybe Text)
getSafePathAdd dir place path = do
  dirpath <- getExpandedPath dir
  checkPath dirpath
  case existsInPath [dirpath, dir] of
      True -> return . Just $ appendPath place . format fp $ dirpath
      _    -> return   Nothing
  where
    appendPath :: Placement -> Text -> Text
    appendPath Cons dir' = dir' <> ":" <> path
    appendPath Snoc dir' = path <> ":" <> dir'

    existsInPath :: [FilePath] -> Bool
    existsInPath dirs = null . filter (`elem` possible) . T.splitOn ":" $ path
      where
        possible :: [Text]
        possible = format fp <$> dirs

getExpandedPath :: MonadIO io => FilePath -> io FilePath
getExpandedPath p = do
  (ExitSuccess, fulldir) <- getFirst <$> shellStrict ("echo " <> format fp p) (return "")
  realpath fulldir
  where
    getFirst = (id *** (fromText . head . T.lines))

data Placement = Cons | Snoc
  deriving Enum

placementFromBool :: Bool -> Placement
placementFromBool = toEnum . fromEnum

type IsFolder = Bool
type IsFile = Bool


checkPath :: (MonadThrow io, MonadIO io) => FilePath -> io ()
checkPath p = liftM2M checkPathGuard (testdir p) (testfile p)


checkPathGuard :: (MonadThrow io, MonadIO io) => IsFolder -> IsFile -> io ()
checkPathGuard True     _ = return ()
checkPathGuard    _  True = throwM FoundFileNotDirectory
checkPathGuard    _ False = throwM InvalidPath


parser :: Parser (FilePath, Bool)
parser = (,)
  <$> argPath "dir" "directory to add to PATH"
  <*> switch "tail" 't' "place the directory at the end of the path (defaults to the front)"


liftM2M :: Monad m => (a -> b -> m c) -> m a -> m b -> m c
liftM2M fn ma mb = do { a <- ma; b <- mb; fn a b }


data SafePathAddExceptions
  = FoundFileNotDirectory
  | InvalidPath
  | NoPATHFound
  deriving (Show, Exception)

test :: IO ()
test = do
  Just path <- need "PATH"
  print ("these should work" :: Text)
  let  canAdd = checkSPA path (Just True)
  let cantAdd = checkSPA path Nothing
  canAdd "."
  canAdd ".."
  cantAdd "/bin"
  cantAdd "/usr/bin"
  cantAdd "$HOME/.local/bin"
  cantAdd "~/.local/bin"

  print ("these should not work" ::Text)
  getSafePathAdd "./foobar" Cons path >>= print
  getSafePathAdd "./stack.yaml" Cons path >>= print
  where
    checkSPA path expect p = do
      rdir <- format fp <$> getExpandedPath p
      out <- getSafePathAdd p Cons path
      let res = not . null . filter (== rdir) . T.splitOn ":" <$> out
      print $ res == expect

