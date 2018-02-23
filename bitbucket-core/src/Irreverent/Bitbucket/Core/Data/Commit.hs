{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Commit
-- Copyright    : (C) 2017 - 2018 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Commit (
  -- * Types
    Commit(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import qualified Ultra.Data.Text as T

import Preamble

data Commit = Commit {
    cHash :: !T.Text
  , cSelf :: !Href
  } deriving (Show, Eq)
