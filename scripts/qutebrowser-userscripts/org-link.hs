#!/run/current-system/sw/bin/runghc

{-# LANGUAGE OverloadedStrings #-}

import System.Environment (lookupEnv)
import Network.HTTP.Base (urlEncode)
import Turtle
import Data.Text (concat, pack)
import Data.Maybe (fromMaybe)

encode str = case str of
  Nothing -> ""
  Just val -> urlEncode val

main = do
  vars <- mapM lookupEnv ["QUTE_URL", "QUTE_TITLE", "QUTE_SELECTED_TEXT"]
  -- print vars
  let [_, title, selected] = map encode vars
  -- print encoded
  let url = fromMaybe "" $ head vars
  let out = "\"org-protocol://capture?template=l&url=" ++ url ++ "&title=" ++ title ++ "&body=" ++ selected ++ "\""
  putStrLn out
  let command = Data.Text.concat ["emacsclient ", Data.Text.pack out]
  shell command empty
