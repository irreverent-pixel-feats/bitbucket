{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Core.Data.PullRequests.PullRequest
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.PullRequests.PullRequest (
  -- * Types
    PullRequest(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common
import Irreverent.Bitbucket.Core.Data.Commit
import Irreverent.Bitbucket.Core.Data.RepositorySummary

import qualified Ultra.Data.Text as T

import Preamble

-- |
-- @"type": "pull_request"@
-- WIP
data PullRequest = PullRequest {
    prTitle :: !Title
  , prDescription :: !Description
  , prCloseSourceBranch :: !PRCloseSourceBranch
  , prMergeCommit :: !(Maybe Commit)
  , prDestinationCommit :: !Commit
  , prDestinationRepository :: !RepositorySummary
  , prDestinationBranch :: !T.Text
  , prState :: !PRState
  , prDecline :: !Href
  , prCommits :: !Href
  , prSelf :: !Href
  , prComments :: !Href
  , prMerge :: !Href
  , prHtml :: !Href
  , prActivity :: !Href
  , prDiff :: !Href
  , prApprove :: !Href
  , prStatuses :: !Href
  , prUsername     :: !(Maybe User)
  , prWebsite     :: !(Maybe Website)  -- ^ apparently this comes as null sometimes...
  , prUuid        :: !Uuid
  } deriving (Show, Eq)
