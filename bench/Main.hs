{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TupleSections #-}

import Control.Monad (forM, replicateM)
import Criterion.Main
import Data.List (intersperse)
import Data.Map qualified as Map
import Data.Set qualified as Set
import System.Directory (findExecutable)
import System.FilePath ((</>))
import System.Process (readProcess)
import System.Random.Stateful (randomRIO)
import XReferee.GitUtils (withGitRepo)

main :: IO ()
main = do
  xreferee <- findExecutable "xreferee" >>= maybe (error "Could not find xreferee") pure
  putStrLn $ "Using executable: " <> xreferee

  putStrLn "Generating files..."
  files <- genFiles

  putStrLn "Initializing git repo..."
  withGitRepo files $
    defaultMain
      [ bench "xreferee-e2e" . whnfIO $ do
          _ <- readProcess xreferee [] ""
          pure ()
      ]

genFiles :: IO [(FilePath, String)]
genFiles = do
  labels <- fmap Set.toList . genSetOf numLabels $ genString (1, 20)
  files <- genFileTree fileTreeMaxDepth
  fileToAnchors <- genRevMap labels $ genOneOf files
  forM files $ \f -> do
    let anchors = Map.findWithDefault [] f fileToAnchors
    refs <- genList (0, 10) $ genOneOf labels
    markers <-
      shuffle . concat $
        [ ["#(ref:" <> label <> ")" | label <- anchors]
        , ["@(ref:" <> label <> ")" | label <- refs]
        ]
    contents <- genFileContents markers
    pure (f, contents)
  where
    numLabels = 1000
    -- 20 * 5^5 = 62500 max files
    fileTreeMaxDepth = 5
    maxSubdirs = 5
    maxFilesPerDir = 20

    genSetOf (n :: Int) r =
      let go 0 acc = pure acc
          go i acc = do
            a <- r
            if a `Set.member` acc
              then go i acc
              else go (i - 1) (Set.insert a acc)
       in go n Set.empty

    genString lenRange = genList lenRange $ genOneOf $ ['a' .. 'z'] <> ['A' .. 'Z'] <> ['0' .. '9'] <> "-_"

    genFileTree = \case
      (0 :: Int) -> pure []
      maxDepth -> do
        files <- genList (0, maxFilesPerDir) $ genString (1, 20)
        dirs <- genList (0, maxSubdirs) $ genString (1, 20)
        let genDir dir = map (dir </>) <$> genFileTree (maxDepth - 1)
        (files <>) <$> concatMapM genDir dirs

    genFileContents markers = do
      numLines <- randomRIO (1, 1000 :: Int)
      lineToMarkers <- genRevMap markers $ randomRIO (1, numLines)
      fmap unlines . forM [1 .. numLines] $ \line -> do
        let lineMarkers = Map.findWithDefault [] line lineToMarkers
        before <- genString (0, 50)
        after <- genString (0, 50)
        parts <- sequence . intersperse (genString (0, 10)) . map pure $ lineMarkers
        pure . concat $ [before] <> parts <> [after]

    genList lenRange r = do
      len <- randomRIO lenRange
      replicateM len r

    shuffle = \case
      [] -> pure []
      [x] -> pure [x]
      xs -> do
        -- Try to give each element a unique rank and sort on the rank.
        -- Recursively shuffle, just in case multiple elements mapped to the same rank
        ranked <- genRevMap xs $ randomRIO (1, length xs * 10)
        concatMapM (shuffle . snd) (Map.toAscList ranked)

    genRevMap keys genVal = Map.fromListWith (<>) <$> mapM (\k -> (,[k]) <$> genVal) keys

    genOneOf xs = do
      i <- randomRIO (0, length xs - 1)
      pure $ xs !! i

    concatMapM f = fmap concat . mapM f
