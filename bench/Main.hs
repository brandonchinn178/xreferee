{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM_)
import Criterion.Main
import Data.List (sortOn)
import Data.Map qualified as Map
import System.Directory (findExecutable, getFileSize)
import System.Process (readProcess)
import XReferee.TestUtils.Fixtures (Fixture (..), getGitFixtures)
import XReferee.TestUtils.Git (withGitRepo)

main :: IO ()
main = do
  xreferee <- findExecutable "xreferee" >>= maybe (error "Could not find xreferee") pure
  putStrLn $ "Using executable: " <> xreferee

  fixtures <- fmap (sortOn snd) $ getGitFixtures >>= mapM addSize

  forM_ fixtures $ \((path, loadFixture), size) -> do
    putStrLn $ replicate 80 '*'
    putStrLn $ path
    fixture <- loadFixture
    putStrLn $ "  - Size: " <> renderSize size
    putStrLn $ "  - Total files: " <> (show . length) fixture.files
    putStrLn $ "  - Total anchors: " <> (show . length) fixture.anchors
    putStrLn $ "  - Total references: " <> (show . length . concat . Map.elems) fixture.refs
    putStrLn $ replicate 80 '*'
    withGitRepo fixture.files $
      defaultMain
        [ bench "xreferee" . whnfIO $ do
            -- should be checked in IntegrationSpec
            _ <- readProcess xreferee [] ""
            pure ()
        ]

addSize :: (FilePath, a) -> IO ((FilePath, a), Integer)
addSize (fp, a) = do
  size <- getFileSize fp
  pure ((fp, a), size)

renderSize :: Integer -> String
renderSize n
  | n < 1000 = show n <> "B"
  | n < 1000000 = show (n `div` 1000) <> "KB"
  | otherwise = show (n `div` 1000000) <> "MB"
