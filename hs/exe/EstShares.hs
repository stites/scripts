#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TupleSections #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import Control.Monad.Catch


main :: IO ()
main = do
  (yours,total,eval,cost) <- options "estimate how much your startup equity will be worth" parser
  let perc = yours / (total * 10^6)
  print $ perc * eval * 10^6 - (yours * (maybe 0.01 id cost))


parser :: Parser (Double, Double, Double, Maybe Double)
parser = (,,,)
  <$> argDouble "yours" "how many shares you have"
  <*> argDouble "total" "how many shares exist, total (in millions)"
  <*> argDouble "evaluation" "how much you estimate the company to be purchased at (in millions)"
  <*> optional (optDouble "cost" 'c' "how much each share costs defaults to $0.01")

