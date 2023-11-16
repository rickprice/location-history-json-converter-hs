{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}

module Model (Location, LocationMaybe, Model) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Prelude

data Location = Location
    { timestamp :: !Text
    , longitudeE7 :: Int
    , latitudeE7 :: Int
    }
    deriving (Show, Eq, Ord, Generic)

data LocationMaybe = LocationMaybe
    { timestamp :: Maybe Text
    , longitudeE7 :: Maybe Int
    , latitudeE7 :: Maybe Int
    }
    deriving (Show, Eq, Ord, Generic)

data Model = Model
    { locations :: [LocationMaybe]
    }
    deriving (Show, Eq, Ord, Generic)

instance ToJSON LocationMaybe
instance FromJSON LocationMaybe
instance ToJSON Model
instance FromJSON Model
