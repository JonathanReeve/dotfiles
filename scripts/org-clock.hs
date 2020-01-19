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
  case out of
    -- Emacs is on, but returns "-1", which means that org-clock is not running.
    (ExitSuccess, "-1\n") -> TIO.putStrLn $ "Not clocked in."
    -- Emacs is on and clocking. Print the clock value.
    (ExitSuccess, out) -> TIO.putStrLn $ T.drop 1 $ head $ T.splitOn "\"" out
    -- Emacs is not on.
    (ExitFailure err, _) -> TIO.putStrLn $ "Emacs off: " <> repr err
