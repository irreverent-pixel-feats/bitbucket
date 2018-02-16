{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Repositories.List
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file etc/LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Repositories.List (
  -- * Functions
    listRepositories
  , repoConduit
  ) where

import Irreverent.Bitbucket.Http.Common
import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Paginated

import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Common (Username(..))
import Irreverent.Bitbucket.Core.Data.Repository (Repository(..))

import Irreverent.Bitbucket.Json.Repository (RepositoryJsonV2(..))

import Ultra.Control.Monad.Trans.Either (EitherT)
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Data.Conduit (($$), (=$=))
import qualified Ultra.Data.Conduit as C
import qualified Ultra.Data.Conduit.List as C
import qualified Ultra.Data.Text as T

import qualified Network.Wreq.Session as S

import Preamble

repoConduit
  :: forall m. (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> C.Producer (EitherT BitbucketAPIError (BitbucketT m)) [Repository]
repoConduit sess owner =
  let
    initReq :: T.Text
    initReq = T.concat [baseV2URL, "/repositories/", getUsername owner, "/"]
  in paginatedGetConduit sess initReq =$= C.map (fmap fromRepositoryJsonV2)

listRepositories
  :: (MonadCatch m, MonadIO m)
  => S.Session
  -> Username
  -> EitherT BitbucketAPIError (BitbucketT m) [Repository]
listRepositories sess owner = join <$> (repoConduit sess owner $$ C.consume)
