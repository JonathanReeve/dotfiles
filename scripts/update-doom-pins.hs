#! /usr/bin/env nix-shell
#! nix-shell -i runhaskell -p "haskellPackages.ghcWithPackages (ps: with ps; [turtle aeson text])" -p gh

{-# LANGUAGE OverloadedStrings #-}

import Turtle
import Prelude hiding (FilePath)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO
import Data.Aeson (decode, Value(..))
import Data.Aeson.Types (parseMaybe)
import qualified Data.Aeson.KeyMap as KM
import qualified Data.Vector as V
import Data.Maybe (fromMaybe, mapMaybe, catMaybes)
import Control.Monad (forM)

-- | Represents a pinned package with its repository and current pin
data PinnedPackage = PinnedPackage
  { pkgName :: Text
  , pkgRepo :: Text  -- owner/repo format
  , pkgPin :: Text
  , pkgLineNum :: Int
  } deriving (Show)

-- | Extract repository owner/name from various recipe formats
extractRepo :: Text -> Maybe Text
extractRepo line
  | ":host github :repo" `T.isInfixOf` line =
      case T.breakOn ":repo" line of
        (_, rest) -> case T.words rest of
          (_:repo:_) -> Just $ T.replace "\"" "" $ T.replace ")" "" repo
          _ -> Nothing
  | ":type git" `T.isInfixOf` line && "github.com" `T.isInfixOf` line =
      case T.breakOn "github.com/" line of
        (_, rest) ->
          let repo = T.takeWhile (/= '"') $ T.drop (T.length "github.com/") rest
              cleaned = T.replace ".git" "" repo
          in if T.null cleaned then Nothing else Just cleaned
        _ -> Nothing
  | otherwise = Nothing

-- | Get the latest commit SHA for a GitHub repository
getLatestCommit :: Text -> IO (Maybe Text)
getLatestCommit repo = do
  let skipRepos = ["git@github.pie.apple.com"]  -- Skip private Apple repos
  if any (`T.isInfixOf` repo) skipRepos
    then return Nothing
    else do
      result <- shellStrict ("gh api repos/" <> repo <> "/commits/HEAD --jq .sha") empty
      case result of
        (ExitSuccess, sha) -> return $ Just $ T.strip sha
        _ -> do
          printf ("Warning: Failed to get latest commit for "%s%"\n") repo
          return Nothing

-- | Parse packages.el to find pinned packages
parsePinnedPackages :: Text -> [PinnedPackage]
parsePinnedPackages content =
  let contentLines = zip [1..] (T.lines content)
      -- Find lines with :pin
      pinLines = [(num, line) | (num, line) <- contentLines, ":pin" `T.isInfixOf` line]

      -- For each pin line, look backwards to find the package name and repo
      findPackageInfo :: (Int, Text) -> Maybe PinnedPackage
      findPackageInfo (lineNum, pinLine) =
        let -- Extract current pin value
            currentPin = case T.breakOn ":pin \"" pinLine of
              (_, rest) -> T.takeWhile (/= '"') $ T.drop 6 rest

            -- Look backwards up to 10 lines to find recipe and package name
            startLine = max 1 (lineNum - 10)
            contextLines = [(n, l) | (n, l) <- contentLines, n >= startLine && n <= lineNum]

            -- Find package declaration
            pkgDecl = [l | (_, l) <- contextLines, "(package!" `T.isInfixOf` l]
            name = case pkgDecl of
              (decl:_) -> case T.words decl of
                (_:n:_) -> T.takeWhile (\c -> c /= ' ' && c /= ')') n
                _ -> ""
              _ -> ""

            -- Find repo in recipe
            recipeLines = [l | (_, l) <- contextLines]
            repo = msum $ map extractRepo recipeLines

        in case repo of
          Just r -> Just $ PinnedPackage name r currentPin lineNum
          Nothing -> Nothing

  in mapMaybe findPackageInfo pinLines

-- | Update a pin in the content
updatePin :: Text -> Int -> Text -> Text -> Text
updatePin content lineNum oldPin newPin =
  let contentLines = T.lines content
      updatedLines = zipWith (\n line ->
        if n == lineNum
          then T.replace oldPin newPin line
          else line) [1..] contentLines
  in T.unlines updatedLines

-- | Main function
main :: IO ()
main = do
  let packagesFile = fromText "/Users/jon/Dotfiles/dotfiles/doom/packages.el"

  -- Check if file exists
  exists <- testfile packagesFile
  unless exists $ do
    printf ("Error: packages.el not found at "%fp%"\n") packagesFile
    exit (ExitFailure 1)

  -- Read packages.el
  content <- TIO.readFile (T.unpack $ format fp packagesFile)

  printf "Parsing packages.el...\n"
  let pinnedPkgs = parsePinnedPackages content

  printf ("Found "%d%" pinned packages:\n") (length pinnedPkgs)
  let pkgList = T.unlines $ map (\pkg ->
        "  - " <> pkgName pkg <> " (pinned to " <> T.take 7 (pkgPin pkg) <> ")") pinnedPkgs
  TIO.putStrLn pkgList

  -- Get latest commits for each package
  printf "Checking for updates...\n"
  updates <- forM pinnedPkgs $ \pkg -> do
    printf ("  "%s%" ("%s%")... ") (pkgName pkg) (pkgRepo pkg)
    latestCommit <- getLatestCommit (pkgRepo pkg)
    case latestCommit of
      Just newSha -> do
        if T.isPrefixOf (pkgPin pkg) newSha || newSha == pkgPin pkg
          then do
            printf "✓ up to date\n"
            return Nothing
          else do
            printf ("📦 update available: "%s%" -> "%s%"\n") (T.take 7 $ pkgPin pkg) (T.take 7 newSha)
            return $ Just (pkg, newSha)
      Nothing -> do
        printf "⊘ skipped (private or inaccessible)\n"
        return Nothing

  -- Apply updates
  let validUpdates = catMaybes updates
  printf "\n"
  printf "═══════════════════════════════════════════════════════════\n"
  if null validUpdates
    then do
      let total = length pinnedPkgs
      printf ("✓ All packages are up to date! ("%d%"/"%d%")\n") total total
    else do
      let numUpdates = length validUpdates
      let total = length pinnedPkgs
      printf ("Summary: Updating "%d%" out of "%d%" packages:\n") numUpdates total
      let updateSummary = T.unlines $ map (\(pkg, newSha) ->
            "  • " <> pkgName pkg <> ": " <> T.take 7 (pkgPin pkg) <> " -> " <> T.take 7 newSha) validUpdates
      TIO.putStrLn updateSummary

      let updatedContent = foldl (\acc (pkg, newSha) ->
            updatePin acc (pkgLineNum pkg) (pkgPin pkg) newSha) content validUpdates

      TIO.writeFile (T.unpack $ format fp packagesFile) updatedContent
      printf "✓ Updated packages.el successfully!\n"
  printf "═══════════════════════════════════════════════════════════\n"
