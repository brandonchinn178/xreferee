{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module XReferee.TestUtils.Fixtures (
  Fixture (..),
  Loc (..),
  getGitFixtures,
) where

import Data.Aeson ((.:))
import Data.Aeson qualified as Aeson
import Data.Aeson.Types qualified as Aeson
import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import System.Directory (
  getCurrentDirectory,
  listDirectory,
 )
import System.FilePath ((</>))

data Loc = Loc
  { file :: FilePath
  , lineNum :: Int
  }

data Fixture = Fixture
  { anchors :: Map Text Loc
  , refs :: Map Text [Loc]
  , files :: [(FilePath, Text)]
  }

getGitFixtures :: IO [(FilePath, IO Fixture)]
getGitFixtures = do
  cwd <- getCurrentDirectory
  let fixturesDir = cwd </> "data/fixtures/"

  fixtures <- map (fixturesDir </>) <$> listDirectory fixturesDir
  pure [(fixture, loadFixture fixture) | fixture <- fixtures]
  where
    loadFixture fp = do
      Just result <- Aeson.decodeFileStrict fp
      either error pure $ Aeson.parseEither parseFixture result

    parseFixture o = do
      anchors <- traverse parseLoc =<< o .: "anchors"
      refs <- traverse (mapM parseLoc) =<< o .: "refs"
      files <- Map.toList <$> o .: "files"
      pure Fixture{..}

    parseLoc o = do
      file <- o .: "file"
      lineNum <- o .: "line_num"
      pure Loc{..}
