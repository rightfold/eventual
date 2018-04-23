{-# LANGUAGE TemplateHaskell #-}

module Eventual.Issue.ListIssues
  ( State
  , initial
  , handle
  , render
  ) where

import Eventual.Attributes
import Eventual.Issue

import Brick ((<+>))
import Control.Lens ((^.), makePrisms)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import qualified Graphics.Vty as V



newtype State n =
  State (B.List n Issue)

$(makePrisms ''State)



initial :: n -> Vector Issue -> State n
initial n issues = State (B.list n issues 1)



handle :: Ord n => V.Event -> State n -> B.EventM n (State n)
handle ev st = B.handleEventLensed st _State B.handleListEvent ev



render :: (Ord n, Show n) => Bool -> State n -> B.Widget n
render focus (State list) =
  B.borderWithLabel (B.str "Issues") $
    B.renderList (const renderIssue) focus list

renderIssue :: Issue -> B.Widget n
renderIssue issue =
  withIssueLevelAttr (issue ^. issueLevel) $
        renderIssueTitle (issue ^. issueTitle)
    <+> renderIssueShortID (issue ^. issueShortID)
    <+> renderIssueEventCount (issue ^. issueEventCount)

renderIssueShortID :: IssueShortID -> B.Widget n
renderIssueShortID (IssueShortID a) =
  B.hLimit 20 . B.padLeft B.Max .
    B.txt $ a

withIssueLevelAttr :: IssueLevel -> B.Widget n -> B.Widget n
withIssueLevelAttr IssueLevelError   = B.withAttr attr_issueLevel_error
withIssueLevelAttr IssueLevelWarning = B.withAttr attr_issueLevel_warning
withIssueLevelAttr IssueLevelDefault = B.withAttr attr_issueLevel_default

renderIssueTitle :: Text -> B.Widget n
renderIssueTitle = B.padRight B.Max . B.txt

renderIssueEventCount :: Word -> B.Widget n
renderIssueEventCount =
  B.hLimit 6 . B.padLeft B.Max .
    B.str . show
