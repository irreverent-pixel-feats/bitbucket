{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Http.Paginated
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Http.Paginated (
  -- * Functions
    paginatedGetConduit
  ) where

import Irreverent.Bitbucket.Http.Error
import Irreverent.Bitbucket.Http.Methods

import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Common (Uri(..))
import Irreverent.Bitbucket.Core.Data.Paginated (Paginated(..))

import Irreverent.Bitbucket.Json.Paginated (PaginatedJsonV2(..))
 
import Ultra.Control.Monad.Catch (MonadCatch(..))
import Ultra.Data.Aeson (FromJSON)

import Ultra.Control.Monad.Trans.Either (EitherT)
import qualified Ultra.Data.Conduit as C
import qualified Ultra.Data.Text as T
import qualified Network.Wreq.Session as S

import Preamble

paginatedGetConduit
  :: forall m a . (FromJSON a, MonadCatch m, MonadIO m)
  => S.Session
  -> T.Text
  -> C.Producer (EitherT BitbucketAPIError (BitbucketT m)) [a]
paginatedGetConduit sess req =
  let
    loop :: Paginated a -> C.Producer (EitherT BitbucketAPIError (BitbucketT m)) [a]
    loop pg = do
      C.yield $ pageValues pg
      next <- forM (nextPage pg) $ \case
        Uri uri -> lift . fmap fromPaginatedJsonV2 $ bitbucketGet sess uri
      maybe (pure ()) loop $ next

  in lift (fmap fromPaginatedJsonV2 $ bitbucketGet sess req) >>= loop
