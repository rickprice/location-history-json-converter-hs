{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE Unsafe #-}

module Model (Location, Model, locations, timestamp, isComplete, toXMLString) where

import Data.Aeson

-- import Data.Text (Text)
import Data.Time.Clock
import GHC.Generics
import Prelude
import Data.Time.Format.ISO8601
-- import Data.List(foldl')

data Location = Location
    { timestamp :: Maybe UTCTime
    , longitudeE7 :: Maybe Int
    , latitudeE7 :: Maybe Int
    , altitude :: Maybe Int
    , accuracy :: Maybe Int
    }
    deriving stock (Show, Eq, Ord)
    deriving stock (Generic)

data Model = Model
    { locations :: [Location]
    }
    deriving stock (Show, Eq, Ord)
    deriving stock (Generic)

instance ToJSON Location
instance FromJSON Location
instance ToJSON Model
instance FromJSON Model

isComplete :: Location -> Bool
isComplete x = case (t, lo, la) of
    (Nothing, _, _) -> False
    (_, Nothing, _) -> False
    (_, _, Nothing) -> False
    (_, _, _) -> True
  where
    t = timestamp x
    lo = longitudeE7 x
    la = latitudeE7 x

-- lastN' n xs = foldl' (const . drop 1) <*> drop n

convertLocation :: Int -> String
convertLocation x = reverse (start ++ "." ++ end) where
    (start,end) = splitAt 7 (reverse $ show x )

xmlGISHeader :: String
xmlGISHeader = "<?xml version=\"1.0\" encoding=\"UTF-8\"?><kml xmlns=\"http://www.opengis.net/kml/2.2\"><Document><name>Location History</name>"

toData :: String -> Maybe Int -> String
toData _ Nothing = ""
toData name (Just x)  = "<Data name=\"" ++ name ++ "\"><value>" ++ show x ++ "</value></Data>"

optionals :: Location -> String
optionals x = mconcat [toData "accuracy" (accuracy x), toData "altitude" (altitude x) ]

extendedData :: String -> String
extendedData x = if null x then "" else "<ExtendedData>" ++ x ++ "</ExtendedData>"

toGISBody :: Location -> String
toGISBody x =
    "<Placemark>"
        ++ "<TimeStamp><when>"
        ++ maybe "" iso8601Show (timestamp x)
        ++ "</when></TimeStamp>"
        ++ extendedData ( optionals x)
        ++ "<Point><coordinates>"
        ++ maybe "" convertLocation (longitudeE7 x)
        ++ ","
        ++ maybe "" convertLocation (latitudeE7 x)
        ++ "</coordinates></Point>"
        ++ "</Placemark>"

xmlGISFooter :: String
xmlGISFooter = "</Document></kml>"

toXMLString :: [Location] -> String
toXMLString x =
    xmlGISHeader
        ++ concatMap toGISBody x
        ++ xmlGISFooter
