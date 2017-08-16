#!/usr/bin/env stack
-- stack --install-ghc runghc --package turtle --package exceptions --package text --package optional-args
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE LambdaCase #-}
module Main where

import Prelude hiding (FilePath)
import Turtle
import qualified Data.Text as T
import Data.Optional (Optional(..))


main :: IO ()
main = do
  (mode, mirror) <- options "choose preconfigured xrandr settings" parser
  case mode of
    Sentenai -> xrandrWithHDMI mirror "3440x1440"

    Home     -> xrandrWithHDMI mirror "3840x1600"

    Off      -> xrandrWith (hdmi2With ["--off"])
             >> xrandrWith (edp1With  ["--mode","1920x1080","--auto"])

    Reset    -> xrandrWith (hdmi2With ["--off"])
             >> xrandrWith (edp1With  ["--off"])
             >> xrandrWith (edp1With  ["--mode","1920x1080","--auto"])
  where
    -- runs xrandr before action so that monitors are detected
    xrandrWith :: [Text] -> IO ()
    xrandrWith args = xrandrWith' [] >> xrandrWith' args

    xrandrWith' :: [Text] -> IO ()
    xrandrWith' args = proc "xrandr" args empty >> pure ()

    xrandrWithHDMI :: Bool -> Text -> IO ()
    xrandrWithHDMI mirror res = xrandrWith $
         hdmi2With ["--auto", "--mode",    res]
      <> edp1With  ["--auto", if mirror then "--same-as" else "--right-of", "HDMI-2"]

    hdmi2With :: [Text] -> [Text]
    hdmi2With = (outputOf "HDMI-2" <>)

    edp1With  :: [Text] -> [Text]
    edp1With  = (outputOf  "eDP-1" <>)

    outputOf  :: Text -> [Text]
    outputOf o' = [ "--output",  o']


data MonitorSettings
  = Sentenai
  | Home
  | Off
  | Reset
  deriving (Eq, Ord, Show, Bounded, Enum)


monitorModes :: Text
monitorModes = T.intercalate "|" $
  fmap (T.toLower . T.pack . show) [minBound..maxBound::MonitorSettings]

help :: Text -> Optional HelpMessage
help = Specific . HelpMessage

parser :: Parser (MonitorSettings, Bool)
parser = (,)
  <$> arg txt2mode "mode" (help $ "mode to change to, one of [" <> monitorModes <> "]")
  <*> switch "mirror" 'm' "have laptop mirror HDMI (defaults to false for xmonad)"
  where
    txt2mode :: Text -> Maybe MonitorSettings
    txt2mode "sentenai" = Just Sentenai
    txt2mode "reset"   = Just Reset
    txt2mode "home"     = Just Home
    txt2mode "off"      = Just Off
    txt2mode _          = Nothing
