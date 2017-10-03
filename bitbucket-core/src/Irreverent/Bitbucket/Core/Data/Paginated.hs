{-# LANGUAGE NoImplicitPrelude #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Core.Data.Paginated
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Core.Data.Paginated (
  -- * Types
    Paginated(..)
  ) where

import Irreverent.Bitbucket.Core.Data.Common

import Preamble
import Prelude (Integer)

data Paginated a = Paginated {
    nextPage :: !(Maybe Uri)
  , prevPage :: !(Maybe Uri)
  , totalPaginatedLength :: !Integer
  , pageNumber :: !Integer
  , pageLength :: !Integer
  , pageValues :: [a]
  } deriving (Show, Eq)

instance Functor Paginated where
--fmap :: (a -> b) -> f a -> f b
  fmap f (Paginated nextPage' prevPage' totalPaginatedLength' pageNumber' pageLength' pageValues') =
    Paginated
      nextPage'
      prevPage'
      totalPaginatedLength'
      pageNumber'
      pageLength'
      (fmap f pageValues')

instance Foldable Paginated where
--foldMap :: (Monoid m) => (a -> m) -> f a -> m
  foldMap f (Paginated _ _ _ _ _ pageValues') =
    foldMap f pageValues'

instance Traversable Paginated where
--traverse :: (Applicative f) => (a -> f b) -> t a -> f (t b)
  traverse f (Paginated next prev size' pageNum pageLength' xs) =
    Paginated next prev size' pageNum pageLength' <$> traverse f xs
