{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.Pipelines.DeleteEnvironmentVariable
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.Pipelines.DeleteEnvironmentVariable (
  -- * Functions
    deletePipelinesEnvironmentVariable
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Methods

import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Common (
    Username(..)
  , RepoName(..)
  , Uuid(..)
  )

import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

deletePipelinesEnvironmentVariable
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> Uuid
  -> EitherT BitbucketAPIError (BitbucketT m) ()
deletePipelinesEnvironmentVariable sess owner reponame uuid =
  let
    endpoint :: T.Text
    endpoint = T.concat [
        baseV2URL
      , "/repositories/"
      , getUsername owner
      , "/"
      , getRepoName reponame
      , "/pipelines_config/variables/"
      , getUuid uuid
      ]
  in bitbucketDelete sess endpoint
