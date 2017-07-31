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
    Off      -> xrandrWith (hdmi2With ["--off"])
             >> xrandrWith (edp1With  ["--mode","1920x1080","--auto"])
  where
    xrandrWith :: [Text] -> IO ()
    xrandrWith args = proc "xrandr" args empty >> pure ()

    xrandrWithExternal :: Text -> IO ()
    xrandrWithExternal res = xrandrWith $
         hdmi2With ["--auto", "--mode",    res]
      <> edp1With  ["--auto", "--same-as", "HDMI-2"]

    hdmi2With :: [Text] -> [Text]
    hdmi2With = (outputOf "HDMI-2" <>)

    edp1With  :: [Text] -> [Text]
    edp1With  = (outputOf  "eDP-1" <>)

    outputOf  :: Text -> [Text]
    outputOf o = [ "--output",  o]


data MonitorSettings
  = Sentenai
  | Home
  | Off
  deriving (Eq, Ord, Show)


parser :: Parser MonitorSettings
parser = arg txt2mode "mode" "mode to change to, one of [sentenai|home|off]"
  where
    txt2mode :: Text -> Maybe MonitorSettings
    txt2mode "sentenai" = Just Sentenai
    txt2mode "home"     = Just Home
    txt2mode "off"      = Just Off
    txt2mode _          = Nothing
