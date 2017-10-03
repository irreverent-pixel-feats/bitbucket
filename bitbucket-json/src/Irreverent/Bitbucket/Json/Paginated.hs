{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE OverloadedStrings #-}
-------------------------------------------------------------------
-- |
-- Module       : Irreverent.Bitbucket.Json.Paginated
-- Copyright    : (C) 2017 Irreverent Pixel Feats
-- License      : BSD-style (see the file /LICENSE.md)
-- Maintainer   : Dom De Re
--
-------------------------------------------------------------------
module Irreverent.Bitbucket.Json.Paginated (
  -- * Types
    PaginatedJsonV2(..)
  ) where

import Irreverent.Bitbucket.Json.Common

import Irreverent.Bitbucket.Core.Data.Paginated

import Ultra.Data.Aeson (FromJSON(..), ToJSON(..), (.:?), (.:), (.=), object)

import Preamble

newtype PaginatedJsonV2 a = PaginatedJsonV2 {
    fromPaginatedJsonV2 :: Paginated a
  } deriving (Show, Eq)

instance (ToJSON a) => ToJSON (PaginatedJsonV2 a) where
--toJSON :: a -> Value
  toJSON (PaginatedJsonV2 (Paginated nextPage' prevPage' totalPaginatedLength' pageNumber' pageLength' pageValues')) =
    object [
      "next" .= (jsonUri <$> nextPage')
    , "previous" .= (jsonUri <$> prevPage')
    , "page" .= pageNumber'
    , "size" .= totalPaginatedLength'
    , "pagelen" .= pageLength'
    , "values" .= pageValues'
    ]

instance (FromJSON a) => FromJSON (PaginatedJsonV2 a) where
--parseJSON :: Value -> Parser a
  parseJSON v = PaginatedJsonV2 <$> do
    o <- parseJSON v
    Paginated
      <$> (o .:? "next" >>= traverse parseUriJson)
      <*> (o .:? "previous" >>= traverse parseUriJson)
      <*> (o .: "size")
      <*> (o .: "page")
      <*> (o .: "pagelen")
      <*> (o .: "values" >>= traverse parseJSON)
