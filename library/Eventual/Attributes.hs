{-# OPTIONS_GHC -Wno-missing-signatures #-}

{-# LANGUAGE NoMonomorphismRestriction #-}

module Eventual.Attributes where

import Data.Monoid ((<>))

attr_issueLevel_error = "issue-level" <> "error"
attr_issueLevel_warning = "issue-level" <> "warning"
attr_issueLevel_default = "issue-level" <> "default"
