{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Control
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Control (
  -- * Types
    BitbucketT(..)
  -- * Functions
  , getAuth
  ) where

import Irreverent.Bitbucket.Core.Data.Auth (Auth)

import Ultra.Control.Monad.Bracket (MonadBracket(..), liftSVBracket)
import Ultra.Control.Monad.Catch (MonadCatch(..), MonadThrow(..))

import Preamble

newtype BitbucketT m a = BitbucketT {
    runBitbucketT :: ReaderT Auth m a
  } deriving (Functor, Applicative, Monad, MonadCatch, MonadThrow, MonadIO, MonadTrans)

instance (MonadBracket m) => MonadBracket (BitbucketT m) where
  svbracket = liftSVBracket BitbucketT runBitbucketT svbracket


getAuth :: (Monad m) => BitbucketT m Auth
getAuth = BitbucketT ask

