{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.SetGroupPrivs
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.SetGroupPrivs (
  -- * Functions
    addGroupPrivilege
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (
    Username(..)
  , RepoSlug(..)
  , GroupName(..)
  , GroupOwner(..)
  , PrivilegeLevel(..)
  )
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Http.Groups.Privileges.Add (setGroupPrivilege)
import Irreverent.Bitbucket.Json.Groups.Privileges (GroupPrivilegeV1Json(..))

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)

import Data.Aeson.Encode.Pretty (Config(..), Indent(..), encodePretty', defConfig)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

prettyCfg :: Config
prettyCfg = defConfig {
    confIndent = Spaces 2,
    confTrailingNewline = True
  }

addGroupPrivilege
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> RepoSlug
  -> GroupOwner
  -> GroupName
  -> PrivilegeLevel
  -> EitherT CliError m ()
addGroupPrivilege auth owner repo gowner grp priv = do
  session <- liftIO S.newSession
  firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    out <- setGroupPrivilege session owner repo gowner grp priv
    liftIO . BSL.hPut stdout . encodePretty' prettyCfg . fmap GroupPrivilegeV1Json $ out
