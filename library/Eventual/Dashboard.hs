{-# LANGUAGE TemplateHaskell #-}

module Eventual.Dashboard
  ( State
  , initial
  , handle
  , render
  ) where

import Brick ((<+>), (<=>))
import Control.Lens ((&), (^.), (%~), (.~), makeLenses)
import Control.Monad (guard)
import Data.Semigroup ((<>))
import Data.Vector (Vector)
import Eventual.Issue (Issue)
import Eventual.Project (Project)

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Eventual.Issue.ListIssues as Issue.ListIssues
import qualified Eventual.Project.ListProjects as Project.ListProjects
import qualified Graphics.Vty as V



data State n =
  State
    { _stateActivePanel  :: Panel
    , _stateListProjects :: Project.ListProjects.State n
    , _stateListIssues   :: Issue.ListIssues.State n }

data Panel
  = ProjectsPanel
  | IssuesPanel
  deriving stock (Bounded, Enum, Eq)

$(makeLenses ''State)

toEnumPanel :: Int -> Maybe Panel
toEnumPanel n = do
  guard $ n >= fromEnum @Panel minBound
  guard $ n <= fromEnum @Panel maxBound
  pure $ toEnum n

succPanel :: Panel -> Panel
succPanel p | p == maxBound = minBound
            | otherwise     = succ p



initial :: (n, Vector Project) -> (n, Vector Issue) -> State n
initial projects issues =
  State
    { _stateActivePanel  = ProjectsPanel
    , _stateListProjects = uncurry Project.ListProjects.initial projects
    , _stateListIssues   = uncurry Issue.ListIssues.initial     issues }



handle :: Ord n => V.Event -> State n -> B.EventM n (State n)
handle ev st =
  case (ev, st ^. stateActivePanel) of
    (V.EvKey (V.KFun n) [], _) | Just p <- toEnumPanel (n - 1) -> pure $ st & stateActivePanel .~ p
    (V.EvKey (V.KChar '\t') [], _) -> pure $ st & stateActivePanel %~ succPanel
    (_, ProjectsPanel) -> B.handleEventLensed st stateListProjects Project.ListProjects.handle ev
    (_, IssuesPanel  ) -> B.handleEventLensed st stateListIssues   Issue.ListIssues.handle     ev



render :: (Ord n, Show n) => State n -> B.Widget n
render st =
  let
    ap :: Panel
    ap = st ^. stateActivePanel
  in
        Project.ListProjects.render (ap == ProjectsPanel) (st ^. stateListProjects)
    <+> Issue.ListIssues.render     (ap == IssuesPanel)   (st ^. stateListIssues)
    <=> B.hBox [ B.borderWithLabel (B.str $ "EVENTUAL-" <> issueID) (B.fill '.')
               | issueID <- ["A73", "49B", "125", "584"] ]
