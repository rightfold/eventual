{-# LANGUAGE TemplateHaskell #-}

module Eventual.Project
  ( -- * Data types
    ProjectSlug (..)
  , Project (..)

    -- * Optics
  , _ProjectSlug
  , projectSlug
  , projectName
  ) where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson ((.:))
import Data.Text (Text)

import qualified Data.Aeson as Aeson

newtype ProjectSlug =
  ProjectSlug Text
  deriving stock (Eq, Ord, Show)

data Project =
  Project
    { _projectSlug :: ProjectSlug
    , _projectName :: Text }

$(makePrisms ''ProjectSlug)
$(makeLenses ''Project)

instance Aeson.FromJSON ProjectSlug where
  parseJSON j = ProjectSlug <$> Aeson.parseJSON j

instance Aeson.FromJSON Project where
  parseJSON = Aeson.withObject "Project" $ \object ->
    Project
      <$> object .: "slug"
      <*> object .: "name"
