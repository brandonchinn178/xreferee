module XReferee.TestUtils.Fixtures (
  getGitFixtures,
) where

import Data.Aeson qualified as Aeson
import Data.Map qualified as Map
import System.Directory (
  getCurrentDirectory,
  listDirectory,
 )
import System.FilePath ((</>))

getGitFixtures :: IO [(FilePath, IO [(FilePath, String)])]
getGitFixtures = do
  cwd <- getCurrentDirectory
  let fixturesDir = cwd </> "data/fixtures/"

  fixtures <- map (fixturesDir </>) <$> listDirectory fixturesDir
  pure [(fixture, loadFixture fixture) | fixture <- fixtures]
  where
    loadFixture fixture = do
      Just files <- Aeson.decodeFileStrict fixture
      pure $ Map.toList files
