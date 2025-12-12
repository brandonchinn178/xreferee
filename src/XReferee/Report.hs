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
  Label (..),
  LabelLoc (..),
  SearchResult (..),
 )

newtype Report = Report {unReport :: [ReportSection]}
type ReportSection = (ReportSectionInfo, Violations)

data ReportSectionInfo = ReportSectionInfo
  { header :: Text
  , fatal :: Bool
  }
type Violations = Map Label [LabelLoc]

makeReport :: SearchResult -> Report
makeReport result = Report . map resolve . unReportTemplate $ reportTemplate
  where
    resolve (info, mkViolations) = (info, mkViolations result)

renderReport :: Report -> Text
renderReport = Text.intercalate "\n\n" . map renderSection . reportViolations
  where
    renderSection (info, violations) =
      Text.intercalate "\n" . concat $
        [ ["========== " <> info.header <> " =========="]
        , concat
            [ label.pretty : ["    " <> renderLoc loc | loc <- locs]
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
    mkViolations result = filterKeys (`Map.notMember` result.anchors) result.references

reportSectionUnusedAnchors :: ReportSectionTemplate
reportSectionUnusedAnchors = (info, mkViolations)
  where
    info =
      ReportSectionInfo
        { header = "Unused anchors"
        , fatal = False
        }
    mkViolations result = filterKeys (`Map.notMember` result.references) result.anchors

reportSectionDuplicateAnchors :: ReportSectionTemplate
reportSectionDuplicateAnchors = (info, mkViolations)
  where
    info =
      ReportSectionInfo
        { header = "Duplicate anchors"
        , fatal = True
        }
    mkViolations result = Map.filter ((> 1) . length) result.anchors

-- Added in containers 0.8
filterKeys :: (k -> Bool) -> Map k a -> Map k a
filterKeys f = Map.filterWithKey (\k _ -> f k)
