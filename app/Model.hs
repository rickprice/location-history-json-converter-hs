{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE DeriveGeneric #-}

module Model where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Prelude
import GHC.Generics

data Location = Location
  { accuracy :: Int,
    timestamp :: Text,
    longitudeE7 :: Int,
    latitudeE7 :: Int
  }
  deriving (Show, Eq, Ord, Generic)

data Model = Model
  { locations :: [Location]
  }
  deriving (Show, Eq, Ord, Generic)

instance ToJSON Location
instance FromJSON Location
instance ToJSON Model
instance FromJSON Model
