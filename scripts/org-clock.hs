#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import System.Posix.Env
import Data.Maybe

getClock :: Text
getClock = "emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)'"

out :: Shell Line
out = inshell getClock empty

main = do
  out <- shellStrict getClock empty
  -- TODO: make it so that this changes automatically depending on the desktop environment
  -- let red str = "<span color=\"#f00\">" <> str <> "</span>"
  currentDesktop <- getEnv "XDG_CURRENT_DESKTOP"
  let desktops = [ "GNOME", "KDE" ] :: [Text]
  let emptyString = "" :: String
  let emptyText = "" :: Text
  let thisDesktop = T.pack $ fromMaybe emptyString currentDesktop
  let red = if thisDesktop `elem` desktops then emptyText else "%{F#f00}"
  case out of
    -- Emacs is on, but returns "-1", which means that org-clock is not running.
    (ExitSuccess, "-1\n") -> TIO.putStrLn $ "Protocolu!\nProtocolu!\nprotocolu"
    -- Emacs is on and clocking. Print the clock value.
    (ExitSuccess, out) -> TIO.putStrLn $ clockstr <> "\n" <> clockstr <> "\nclock" where
      clockstr = T.splitOn "\"" out !! 1
    -- Emacs is not on.
    (ExitFailure err, _) -> do
      TIO.putStrLn $ "Ensalutu!\nEnsalutu\nensalutu" -- <> repr err
      -- clockVal <- getEnv "clock"
      -- let clock = (read (fromMaybe "0" clockVal)) :: Int
      -- setEnv "clock" (show (clock + 1)) True
      -- clockVal <- getEnv "clock"
      -- let clock = (read (fromMaybe "0" clockVal)) :: Int
      -- print clock

      -- TODO: record how long I've been clocked out,
      -- and prompt me to log in if it's been too long
      -- let countFile = "/tmp/unclocked-count"
      -- -- count <- input countFile
      -- output countFile ((input countFile) + 1)
