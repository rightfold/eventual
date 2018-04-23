{-# LANGUAGE TemplateHaskell #-}

module Eventual.Issue
  ( -- * Data types
    IssueID (..)
  , IssueShortID (..)
  , IssueLevel (..)
  , Issue (..)

    -- * Optics
  , _IssueID
  , _IssueShortID
  , _IssueLevelError
  , _IssueLevelWarning
  , issueID
  , issueShortID
  , issueLevel
  , issueTitle
  , issueEventCount
  ) where

import Control.Lens (makeLenses, makePrisms)
import Data.Aeson ((.:))
import Data.Semigroup ((<>))
import Data.Text (Text)

import qualified Data.Aeson as Aeson

newtype IssueID = IssueID Text
  deriving stock (Eq, Ord, Show)

newtype IssueShortID = IssueShortID Text
  deriving stock (Eq, Ord, Show)

data IssueLevel
  = IssueLevelError
  | IssueLevelWarning
  | IssueLevelDefault
  deriving stock (Eq, Ord, Show)

data Issue =
  Issue
    { _issueID         :: IssueID
    , _issueShortID    :: IssueShortID
    , _issueLevel      :: IssueLevel
    , _issueTitle      :: Text
    , _issueEventCount :: Word
    }

$(makePrisms ''IssueID)
$(makePrisms ''IssueShortID)
$(makePrisms ''IssueLevel)
$(makeLenses ''Issue)

instance Aeson.FromJSON IssueID where
  parseJSON j = IssueID <$> Aeson.parseJSON j

instance Aeson.FromJSON IssueShortID where
  parseJSON j = IssueShortID <$> Aeson.parseJSON j

instance Aeson.FromJSON IssueLevel where
  parseJSON (Aeson.String "error"  ) = pure IssueLevelError
  parseJSON (Aeson.String "warning") = pure IssueLevelWarning
  parseJSON (Aeson.String "default") = pure IssueLevelDefault
  parseJSON j = fail $ "Invalid issue type: " <> show j

instance Aeson.FromJSON Issue where
  parseJSON = Aeson.withObject "Issue" $ \object ->
    Issue
      <$> object .: "id"
      <*> object .: "shortId"
      <*> object .: "type"
      <*> object .: "title"
      <*> object .: "count" <&> read
    where (<&>) = flip (<$>)
