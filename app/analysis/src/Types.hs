{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Applicative
import           Data.Aeson
import           Data.Csv            (DefaultOrdered (headerOrder),
                                      FromField (parseField),
                                      FromNamedRecord (parseNamedRecord),
                                      Header, ToField (toField),
                                      ToNamedRecord (toNamedRecord), (.:), (.=))
import qualified Data.Csv            as Csv
import qualified Data.Text           as T
import qualified Data.Vector         as V

--------------------------------------------------------------------------------

data Company =
  Company
    { cId          :: Int
    , cName        :: T.Text
    , cUrl         :: T.Text
    , cType        :: T.Text
    , cCountry     :: T.Text
    , cCity        :: T.Text
    , cArea        :: T.Text
    , cDescription :: T.Text
    , cComment     :: T.Text
    } deriving (Eq, Show)

instance FromNamedRecord Company where
  parseNamedRecord m =
    Company
      <$> m Csv..: "N"
      <*> m Csv..: "Name"
      <*> m Csv..: "Url"
      <*> m Csv..: "Type"
      <*> m Csv..: "Country"
      <*> m Csv..: "City"
      <*> m Csv..: "Area"
      <*> m Csv..: "Description"
      <*> m Csv..: "Comment"
