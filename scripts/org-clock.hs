#!/run/current-system/sw/bin/runhaskell

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

getClock :: Text
getClock = "emacsclient --eval '(if (org-clocking-p)(org-clock-get-clock-string) -1)'"

out :: Shell Line
out = inshell getClock empty

main = do
  out <- shellStrict getClock empty
  -- TODO: make it so that this changes automatically depending on the desktop environment
  -- let red = "%{F#f00}"
  let red str = "<span color=\"#f00\">" <> str <> "</span>"
  case out of
    -- Emacs is on, but returns "-1", which means that org-clock is not running.
    (ExitSuccess, "-1\n") -> TIO.putStrLn $ red "Protocolu!"
    -- Emacs is on and clocking. Print the clock value.
    (ExitSuccess, out) -> TIO.putStrLn $ T.take 25 $ T.drop 1 $ head $
      drop 1 $ T.splitOn "\"" out
    -- Emacs is not on.
    (ExitFailure err, _) -> TIO.putStrLn $ red "Ensalutu!" -- <> repr err
