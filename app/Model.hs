{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}

module Model where

import Data.Aeson (FromJSON (..), ToJSON (..), Value (..), object, (.:), (.=))
import Data.Aeson.Types (prependFailure, typeMismatch)
import Data.Text (Text)
import GHC.Generics
import Prelude

data Location = Location
    { timestamp :: Maybe Text
    , longitudeE7 :: Maybe Int
    , latitudeE7 :: Maybe Int
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
