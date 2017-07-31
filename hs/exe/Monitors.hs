#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import Control.Monad.Catch


main :: IO ()
main =
  options "choose preconfigured xrandr settings" parser
  >>= \case
    Sentenai -> xrandrWithExternal "3440x1440"
    Home     -> xrandrWithExternal "3840x1600"
    Off      -> xrandrWith ["--output","HDMI-2","--off"]
             >> xrandrWith ["--output","eDP-1","--mode","1920x1080","--auto"]
  where
    xrandrWith args = proc "xrandr" args empty >> pure ()

    xrandrWithExternal res = xrandrWith
      [ "--output", "HDMI-2", "--auto", "--mode",    res
      , "--output", "eDP-1",  "--auto", "--same-as", "HDMI-2"
      ]


data MonitorSettings
  = Sentenai
  | Off
  | Home
  deriving (Eq, Ord, Show)


parser :: Parser MonitorSettings
parser = arg txt2mode "mode" "mode to change to, one of [sentenai|home|off]"
  where
    txt2mode :: Text -> Maybe MonitorSettings
    txt2mode "sentenai" = Just Sentenai
    txt2mode "home"     = Just Home
    txt2mode "off"      = Just Off
    txt2mode _          = Nothing
