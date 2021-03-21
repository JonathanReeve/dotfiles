#!/usr/bin/env runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getClock :: Text
getClock = "/run/current-system/sw/bin/emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)'"

out :: Shell Line
out = inshell getClock empty

main = do
  out <- shellStrict getClock empty
  -- TODO: make it so that this changes automatically depending on the desktop environment
  -- let red str = "<span color=\"#f00\">" <> str <> "</span>"
  let red str = "%{F#f00}" <> str
  case out of
    -- Emacs is on, but returns "-1", which means that org-clock is not running.
    (ExitSuccess, "-1\n") -> TIO.putStrLn $ red "Protocolu!"
    -- Emacs is on and clocking. Print the clock value.
    (ExitSuccess, out) -> TIO.putStrLn $ T.take 25 $ T.drop 1 $
      T.splitOn "\"" out !! 1
    -- Emacs is not on.
    (ExitFailure err, _) -> do
      TIO.putStrLn $ red "Ensalutu!" -- <> repr err
      -- TODO: record how long I've been clocked out,
      -- and prompt me to log in if it's been too long
      let countFile = "/tmp/unclocked-count"
      -- count <- input countFile
      output countFile ((input countFile) + 1)
