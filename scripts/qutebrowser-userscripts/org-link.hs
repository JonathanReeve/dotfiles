#!/home/jon/.nix-profile/bin/runhaskell

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (lookupEnv)
import Network.HTTP.Base (urlEncode)
import Turtle
import Data.Text (concat, pack)

encode str = case str of
  Nothing -> ""
  Just val -> urlEncode val

main = do
  vars <- mapM lookupEnv ["QUTE_URL", "QUTE_TITLE", "QUTE_SELECTED_TEXT"]
  -- print vars
  let encoded = map encode vars
  let [url, title, selected] = encoded
  -- print encoded
  let out = "org-protocol://capture?template=l&url=" ++ url ++ "&title=" ++ title ++ "&body=" ++ selected
  putStrLn out
  let command = Data.Text.concat ["emacsclient ", Data.Text.pack out]
  shell command empty
