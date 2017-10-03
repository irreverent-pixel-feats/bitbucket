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
  -- * Types
    ListReposError(..)
  -- * Functions
  , listRepos
  , renderListReposError
  ) where

import Irreverent.Bitbucket.Core (Username(..), RepoDescription(..), RepoName(..), Repository(..), User(..))
import Irreverent.Bitbucket.Core.Control (BitbucketT(..))
import Irreverent.Bitbucket.Core.Data.Auth (Auth(..))
import Irreverent.Bitbucket.Http.Repositories.List (BitbucketAPIError, renderBitbucketAPIError, listRepositories)

import Ultra.Control.Monad.Trans.Either (EitherT, pattern EitherT, firstEitherT, mapEitherT, runEitherT)
import Ultra.Data.List.NonEmpty (sortBy)
import qualified Ultra.Data.Text as T

import Data.List (replicate)

import qualified Network.Wreq.Session as S

import System.IO (hPutStr, hPutStrLn, stdout)

import Text.Printf (printf)

import Preamble

data ListReposError =
  ListRepoAPIFail !BitbucketAPIError
  deriving (Show)

renderListReposError :: ListReposError -> T.Text
renderListReposError (ListRepoAPIFail e) = renderBitbucketAPIError e

listRepos
  :: (MonadIO m)
  => Auth
  -> Username
  -> EitherT ListReposError m ()
listRepos auth owner = firstEitherT ListRepoAPIFail . EitherT . liftIO . S.withSession $ \session ->
  runEitherT . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    repos <- listRepositories session owner
    let h = printf (T.unpack $ formatString repos) (t "creator") (t "slug") (t "description")
    let b = replicate (length h - 1) '-'
    liftIO $ hPutStr stdout h >> hPutStrLn stdout b
    forM_ repos $ \repo -> lift . lift $
      printf
        (T.unpack $ formatString repos)
        (maybe "None" (getUsername . userName) . repoCreator $ repo)
        (getRepoName . repoName $ repo)
        (getDescription . repoDescription $ repo)
    -- runEitherT . mapEitherT (flip runReaderT auth . runBitbucketT) $ do
    --   repoConduit session owner $$ listSink

biggestLengthBy :: [a] -> (a -> Int) -> Int -> Int
biggestLengthBy xs f def = maybe def (head . sortBy (flip compare)) . nonEmpty . fmap f $ xs

columnFormatSpecifier :: Int -> T.Text
columnFormatSpecifier n = T.concat [" %-", T.pack . show $ n, "s "]

formatString :: [Repository] -> T.Text
formatString rs = flip T.snoc '\n' . T.intercalate "|" . fmap (columnFormatSpecifier . uncurry (biggestLengthBy rs) . fmap T.length) $ [
    (T.length . maybe "None" (getUsername . userName) . repoCreator, "creator")
  , (T.length . getRepoName . repoName, "slug")
  , (T.length . getDescription . repoDescription, "description")
  ]

t :: T.Text -> T.Text
t = id
