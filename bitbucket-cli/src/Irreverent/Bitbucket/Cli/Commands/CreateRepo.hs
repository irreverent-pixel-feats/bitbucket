{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.CreateRepo
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.CreateRepo (
  -- * Functions
    newRepo
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (Username(..), RepoName(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Core.Data.NewRepository (NewRepository(..))
import Irreverent.Bitbucket.Http.Repositories.New (createRepo)
import Irreverent.Bitbucket.Json.Repository (RepositoryJsonV2(..))

import Ultra.Control.Monad.Trans.Either (EitherT, pattern EitherT, firstEitherT, mapEitherT, runEitherT)

import Data.Aeson.Encode.Pretty (encodePretty)
import qualified Data.ByteString.Lazy as BSL
import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

newRepo
  :: (MonadIO m)
  => Auth
  -> Username
  -> RepoName
  -> NewRepository
  -> EitherT CliError m ()
newRepo auth owner rname repo = firstEitherT BitbucketAPIFail . EitherT . liftIO . S.withSession $ \s ->
  runEitherT . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    createRepo s owner rname repo >>= liftIO . BSL.hPutStr stdout . encodePretty . RepositoryJsonV2

