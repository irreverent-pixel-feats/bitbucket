{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE PatternSynonyms #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Cli.Commands.ListRepos
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Cli.Commands.ListRepos (
  -- * Functions
    listRepos
  ) where

import Irreverent.Bitbucket.Cli.Error

import Irreverent.Bitbucket.Core (
    Username(..)
  , Project(..)
  , ProjectName(..)
  , RepoDescription(..)
  , RepoName(..)
  , Repository(..)
  )
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Http.Error (BitbucketAPIError)
import Irreverent.Bitbucket.Http.Repositories.List (repoConduit)

import Ultra.Control.Monad.Catch (MonadCatch)
import Ultra.Control.Monad.Trans.Either (EitherT, firstEitherT, mapEitherT)
import Ultra.Data.Conduit (Conduit, ($$), (=$=), awaitForever,  yield)
import qualified Ultra.Data.Conduit.List as C
import qualified Ultra.Data.Conduit.Binary as C
import qualified Ultra.Data.Text as T
import qualified Ultra.Data.Text.Encoding as T

import qualified Data.ByteString as BS

import qualified Network.Wreq.Session as S

import System.IO (stdout)

import Preamble

data ListReposError =
  ListRepoAPIFail !BitbucketAPIError
  deriving (Show)

renderPipe
  :: (Monad m)
  => Conduit Repository m BS.ByteString
renderPipe = C.map $ \r -> T.encodeUtf8 . T.concat $ [
    getRepoName . repoName $ r
  , maybe "" (T.bracketed " (" ")" . getProjectName . projectName) . repoProject $ r
  , "    : "
  , getDescription . repoDescription $ r
  , "\n"
  ]

listRepos
  :: (MonadCatch m, MonadIO m)
  => Auth
  -> Username
  -> EitherT CliError m ()
listRepos auth owner = firstEitherT BitbucketAPIFail . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
  session <- liftIO S.newSession
  (repoConduit session owner =$= awaitForever (traverse yield) =$= renderPipe $$ C.sinkHandle stdout)
