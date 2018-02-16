{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.AddAccessKey
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.AddAccessKey (
  -- * Functions
    addAccessKey
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Methods

import Irreverent.Bitbucket.Core.Data.Common (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT)
import Irreverent.Bitbucket.Core.Data.AccessKey (AccessKey(..))
import Irreverent.Bitbucket.Core.Data.NewAccessKey (NewAccessKey(..))
import Irreverent.Bitbucket.Json.AccessKey (accessKeyFromJson)
import Irreverent.Bitbucket.Json.NewAccessKey (NewAccessKeyJsonV1(..))

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

addAccessKey
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoName
  -> NewAccessKey
  -> EitherT BitbucketAPIError (BitbucketT m) AccessKey
addAccessKey sess (Username owner) (RepoName repo) key =
  let
    endpoint :: T.Text
    endpoint = T.concat [
        baseV1URL
      , "/repositories/"
      , owner
      , "/"
      , repo
      , "/deploy-keys"
      ]
  in accessKeyFromJson <$> bitbucketPostJson sess endpoint (NewAccessKeyJsonV1 key)
