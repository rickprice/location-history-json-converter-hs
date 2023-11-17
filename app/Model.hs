{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}
{-# LANGUAGE DerivingStrategies #-}

module Model (LocationMaybe, Model, locations, timestamp, isComplete) where

import Data.Aeson
import Data.Text (Text)
import GHC.Generics
import Prelude
import Data.Time.Clock

-- data Location = Location
--     { timestamp :: !Text
--     , longitudeE7 :: Int
--     , latitudeE7 :: Int
--     }
--     deriving stock (Show, Eq, Ord)
--     deriving stock (Generic)

data LocationMaybe = LocationMaybe
    { timestamp :: Maybe UTCTime
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


isComplete :: LocationMaybe -> Bool
isComplete x = case (t,lo,la) of
                (Nothing,_,_) -> False
                (_,Nothing,_) -> False
                (_,_,Nothing) -> False
                (_,_,_) -> True
                where
                    t = timestamp x
                    lo =longitudeE7 x
                    la =latitudeE7 x
