{-# LANGUAGE OverloadedStrings #-}

module Types where

import           Control.Applicative
import           Data.Aeson
import qualified Data.ByteString.Lazy.Char8 as LBS
import           Data.Csv                   (DefaultOrdered (headerOrder),
                                             FromField (parseField),
                                             FromNamedRecord (parseNamedRecord),
                                             Header, ToField (toField),
                                             ToNamedRecord (toNamedRecord),
                                             (.:), (.=))
import qualified Data.Csv                   as Csv
import qualified Data.Text                  as T
import qualified Data.Vector                as V

--------------------------------------------------------------------------------

data Company =
  Company
    { cId          :: Int
    , cName        :: T.Text
    , cUrl         :: String
    , cType        :: T.Text
    , cCountry     :: T.Text
    , cCity        :: T.Text
    , cArea        :: T.Text
    , cDescription :: T.Text
    , cComment     :: T.Text
    , cComment2    :: T.Text
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
      <*> m Csv..: "Comment2"


decodeCompanies :: LBS.ByteString -> Either String (V.Vector Company)
decodeCompanies = fmap snd . Csv.decodeByName


-- data CompanyInfo =
--   CompanyInfo
--     { socialLinkedin :: T.Text
--     , socialFacebook :: T.Text
--     , socialTwitter  :: T.Text
--     , branches       ::

--     } deriving (Eq, Show)

-- data CompanyBranch =
--   CompanyBranch
--     { companyId :: Int

--     }
