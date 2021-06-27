#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (ps: with ps; [http-conduit http-client-tls yaml])"

{-# LANGUAGE OverloadedStrings #-}
import           Data.Aeson            (Value, Array, Object, (.:))
import qualified Data.ByteString.Char8 as L8
import           Data.ByteString.Internal as BS
import           Network.HTTP.Simple
import           System.Environment
-- import qualified Data.Yaml             as Yaml



main :: IO ()
main = do
    args <- getArgs
    let query = Just $ BS.packChars $ head args
    let request
            = setRequestQueryString [("action", Just "wbsearchentities")
                                    ,("search", query)
                                    ,("language", Just "en")
                                    ,("format", Just "json")]
            "https://www.wikidata.org/w/api.php"
    response <- httpJSON request
    let body = (getResponseBody response) :: Object
    let result = body .: "search"
    print result
    -- L8.putStrLn $ Yaml.encode result
