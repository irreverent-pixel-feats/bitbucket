{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Group.Privileges.Add
-- Copyright    : (C) 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Groups.Privileges.Add (
  -- * Functions
    setGroupPrivilege
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Methods

import Irreverent.Bitbucket.Core.Data.Common (
    GroupName(..)
  , GroupOwner(..)
  , Username(..)
  , RepoSlug(..)
  , PrivilegeLevel(..)
  , renderPrivilegeLevel
  )
import Irreverent.Bitbucket.Core.Data.Groups.Privileges (GroupPrivilegeV1(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT)
import Irreverent.Bitbucket.Json.Groups.Privileges (fromGroupPrivilegeV1Json)

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

setGroupPrivilege
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> RepoSlug
  -> GroupOwner
  -> GroupName
  -> PrivilegeLevel
  -> EitherT BitbucketAPIError (BitbucketT m) [GroupPrivilegeV1]
setGroupPrivilege sess (Username owner) (RepoSlug repo) (GroupOwner gowner) (GroupName grp) priv =
  let
    endpoint :: T.Text
    endpoint = T.concat [
        baseV1URL
      , "/group-privileges/"
      , owner
      , "/"
      , repo
      , "/"
      , gowner
      , "/"
      , grp
      ]
  in do
    resp <- bitbucketPutJsonResp sess endpoint (renderPrivilegeLevel priv)
    pure (fromGroupPrivilegeV1Json <$> resp)
