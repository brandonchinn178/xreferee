{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM_)
import Criterion.Main
import Data.List (sortOn)
import System.Directory (findExecutable, getFileSize)
import System.Process (readProcess)
import XReferee.GitUtils (getGitFixtures, withGitRepo)

main :: IO ()
main = do
  xreferee <- findExecutable "xreferee" >>= maybe (error "Could not find xreferee") pure
  putStrLn $ "Using executable: " <> xreferee

  fixtures <- fmap (sortOn snd) $ getGitFixtures >>= mapM addSize

  forM_ fixtures $ \((fixture, loadFixture), size) -> do
    files <- loadFixture
    let label = fixture <> " (num_files=" <> show (length files) <> ", size=" <> renderSize size <> ")"
    withGitRepo files $
      defaultMain
        [ bench label . whnfIO $ do
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
