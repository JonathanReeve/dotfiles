#!/usr/bin/runhaskell
{-# LANGUAGE OverloadedStrings #-}

import Turtle
import qualified Control.Foldl as Fold
import Data.List.Split (splitOn)
import qualified Data.Text as T

translate :: Maybe Line -> Text
translate response = case lineToText <$> response of
 Nothing -> "Got nothin"
 Just "-1" -> "Got error nothin"
 Just clock -> ((T.splitOn "\"" clock) !! 1)

getClock :: Text
getClock = "emacsclient --eval '(org-clock-get-clock-string)'"

out :: Shell Line
out = inshell getClock empty

runcmd cmd = fold (inshell cmd empty) Fold.head

main = do
  outCode <- shell getClock empty
  emacsOut <- runcmd getClock
  case outCode of
    ExitSuccess -> return $ translate emacsOut
    ExitFailure e -> return $ "Failed with code: " <> repr e
