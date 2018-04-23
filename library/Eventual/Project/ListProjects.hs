{-# LANGUAGE TemplateHaskell #-}

module Eventual.Project.ListProjects
  ( State
  , initial
  , handle
  , render
  ) where

import Eventual.Project

import Control.Lens ((&), (^.), (?~), makePrisms)
import Data.Bool (bool)
import Data.Text (Text)
import Data.Vector (Vector)

import qualified Brick as B
import qualified Brick.Widgets.Border as B
import qualified Brick.Widgets.List as B
import qualified Graphics.Vty as V



newtype State n =
  State (B.List n Project)

$(makePrisms ''State)



initial :: n -> Vector Project -> State n
initial n projects = State (B.list n projects 1 & B.listSelectedL ?~ 1)



handle :: Ord n => V.Event -> State n -> B.EventM n (State n)
handle ev st = B.handleEventLensed st _State B.handleListEvent ev



render :: (Ord n, Show n) => Bool -> State n -> B.Widget n
render focus (State list) =
  B.borderWithLabel (B.str "Projects") $
    B.renderList (\b -> bool id renderWithHighlight b . renderProject) focus list

renderWithHighlight :: B.Widget n -> B.Widget n
renderWithHighlight = B.border

renderProject :: Project -> B.Widget n
renderProject project =
  renderProjectName (project ^. projectName)

renderProjectName :: Text -> B.Widget n
renderProjectName = B.txt
