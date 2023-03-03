{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import Prelude

data Locations = Locations
  { locationsAccuracy :: Int,
    locationsTimestamp :: Text,
    locationsLongitudeE7 :: Int,
    locationsLatitudeE7 :: Int
  }
  deriving (Show, Eq, Ord)

data Model = Model
  { modelLocations :: [Locations]
  }
  deriving (Show, Eq, Ord)

instance ToJSON Locations where
  toJSON Locations {..} =
    object
      [ "accuracy" .= locationsAccuracy,
        "timestamp" .= locationsTimestamp,
        "longitudeE7" .= locationsLongitudeE7,
        "latitudeE7" .= locationsLatitudeE7
      ]

instance ToJSON Model where
  toJSON Model {..} =
    object
      [ "locations" .= modelLocations
      ]

instance FromJSON Locations where
  parseJSON (Object v) = do
    locationsAccuracy <- v .: "accuracy"
    locationsTimestamp <- v .: "timestamp"
    locationsLongitudeE7 <- v .: "longitudeE7"
    locationsLatitudeE7 <- v .: "latitudeE7"
    pure $ Locations {..}
  parseJSON invalid = do
    prependFailure
      "parsing Locations failed, "
      (typeMismatch "Object" invalid)

instance FromJSON Model where
  parseJSON (Object v) = do
    modelLocations <- v .: "locations"
    pure $ Model {..}
  parseJSON invalid = do
    prependFailure
      "parsing Model failed, "
      (typeMismatch "Object" invalid)
