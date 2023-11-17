{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DerivingStrategies #-}

module Model (Location, LocationMaybe, Model, locations) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Prelude

data Location = Location
    { timestamp :: !Text
    , longitudeE7 :: Int
    , latitudeE7 :: Int
    }
    deriving stock (Show, Eq, Ord)
    deriving stock (Generic)

data LocationMaybe = LocationMaybe
    { timestamp :: Maybe Text
    , longitudeE7 :: Maybe Int
    , latitudeE7 :: Maybe Int
    }
    deriving stock (Show, Eq, Ord)
    deriving stock (Generic)

data Model = Model
    { locations :: [LocationMaybe]
    }
    deriving stock (Show, Eq, Ord)
    deriving stock (Generic)

instance ToJSON LocationMaybe
instance FromJSON LocationMaybe
instance ToJSON Model
instance FromJSON Model
