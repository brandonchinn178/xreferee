{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}

module XReferee.Report (
  Report,
  makeReport,
  renderReport,
  reportFailure,
) where

import Data.Map (Map)
import Data.Map qualified as Map
import Data.Text (Text)
import Data.Text qualified as Text
import XReferee.SearchResult (
  Label,
  LabelLoc (..),
  SearchResult (..),
  renderLabel,
  toLabel,
 )

newtype Report = Report {unReport :: [ReportSection]}
type ReportSection = (ReportSectionInfo, Violations)

data ReportSectionInfo = ReportSectionInfo
  { header :: Text
  , fatal :: Bool
  }
type Violations = Map Text [LabelLoc]

makeReport :: SearchResult -> Report
makeReport result = Report . map resolve . unReportTemplate $ reportTemplate
  where
    resolve (info, mkViolations) = (info, mkViolations result)

renderReport :: Report -> Text
renderReport = Text.intercalate "\n" . map renderSection . reportViolations
  where
    renderSection (info, violations) =
      Text.unlines . concat $
        [ ["========== " <> info.header <> " =========="]
        , concat
            [ label : ["    " <> renderLoc loc | loc <- locs]
            | (label, locs) <- Map.toList violations
            ]
        ]
    renderLoc loc = Text.pack loc.filepath <> ":" <> (Text.pack . show) loc.lineNum

reportFailure :: Report -> Bool
reportFailure = any (\(info, _) -> info.fatal) . reportViolations

reportViolations :: Report -> [ReportSection]
reportViolations = filter (not . null . snd) . unReport

{----- Report template -----}

newtype ReportTemplate = ReportTemplate {unReportTemplate :: [ReportSectionTemplate]}
type ReportSectionTemplate = (ReportSectionInfo, SearchResult -> Violations)

reportTemplate :: ReportTemplate
reportTemplate =
  ReportTemplate
    [ reportSectionBrokenRef
    , reportSectionDuplicateAnchors
    , reportSectionUnusedAnchors
    ]

reportSectionBrokenRef :: ReportSectionTemplate
reportSectionBrokenRef = (info, mkViolations)
  where
    info =
      ReportSectionInfo
        { header = "Broken references"
        , fatal = True
        }
    mkViolations result =
      renderLabelMap $ excludeKeysFrom result.anchors result.references

reportSectionUnusedAnchors :: ReportSectionTemplate
reportSectionUnusedAnchors = (info, mkViolations)
  where
    info =
      ReportSectionInfo
        { header = "Unused anchors"
        , fatal = False
        }
    mkViolations result =
      renderLabelMap $ excludeKeysFrom result.references result.anchors

reportSectionDuplicateAnchors :: ReportSectionTemplate
reportSectionDuplicateAnchors = (info, mkViolations)
  where
    info =
      ReportSectionInfo
        { header = "Duplicate anchors"
        , fatal = True
        }
    mkViolations result =
      renderLabelMap $ Map.filter ((> 1) . length) result.anchors

excludeKeysFrom :: (Label a, Label b) => Map a v -> Map b v -> Map b v
excludeKeysFrom excludes = filterKeys ((`Map.notMember` Map.mapKeys toLabel excludes) . toLabel)

renderLabelMap :: (Label a) => Map a v -> Map Text v
renderLabelMap = Map.mapKeys renderLabel

-- Added in containers 0.8
filterKeys :: (k -> Bool) -> Map k a -> Map k a
filterKeys f = Map.filterWithKey (\k _ -> f k)
