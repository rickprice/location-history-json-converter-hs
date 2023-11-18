{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}

module Model (LocationMaybe, Model, locations, timestamp, isComplete, toXMLString) where

import Data.Aeson

-- import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Prelude

data LocationMaybe = LocationMaybe
    { timestamp :: Maybe UTCTime
    , longitudeE7 :: Maybe Int
    , latitudeE7 :: Maybe Int
    , altitude :: Maybe Int
    , accuracy :: Maybe Int
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
isComplete x = case (t, lo, la) of
    (Nothing, _, _) -> False
    (_, Nothing, _) -> False
    (_, _, Nothing) -> False
    (_, _, _) -> True
  where
    t = timestamp x
    lo = longitudeE7 x
    la = latitudeE7 x

xmlGISHeader :: String
xmlGISHeader = "<?xml Nothing -version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

toGISBody :: LocationMaybe -> String
toGISBody x =
    "<Placemark>"
        ++ "<TimeStamp><when>"
        ++ maybe "" show (timestamp x)
        ++ "</when></TimeStamp>"
        ++ "<Point><coordinates>"
        ++ maybe "" show (longitudeE7 x)
        ++ ","
        ++ maybe "" show (latitudeE7 x)
        ++ "</coordinates></Point>"
        ++ "</Placemark>"

xmlGISFooter :: String
xmlGISFooter = "</Document></kml>"

toXMLString :: [LocationMaybe] -> String
toXMLString x =
    xmlGISHeader
        ++ concatMap toGISBody x
        ++ xmlGISFooter
