{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE DuplicateRecordFields      #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE StandaloneDeriving         #-}
{-# LANGUAGE BangPatterns               #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE QuasiQuotes                #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Types
  ( ConnectConfig(..)
  , mkConnInfo
  ) where

import           Data.Aeson
import           Data.ByteString.Builder
import           Data.Either.Utils                          (fromRight)
import           Data.Word
import           Data.Typeable
import           Database.PostgreSQL.Simple
import           Database.PostgreSQL.Simple.FromField
import           Database.PostgreSQL.Simple.FromRow
import           Database.PostgreSQL.Simple.ToField
import           Database.PostgreSQL.Simple.ToRow
import qualified Database.PostgreSQL.Simple.TypeInfo.Static as TI
import           GHC.Generics

--------------------------------------------------------------------------------

data ConnectConfig = ConnectConfig
  { host :: String
  , port :: String
  , dbs  :: String
  , user :: String
  , pass :: String
  } deriving (Generic, Eq, Read, Show, Typeable, ToJSON)

mkConnInfo :: ConnectConfig -- ^ Internal configuration representation
           -> ConnectInfo   -- ^ Representation used by Postgresql.Simple
mkConnInfo config =
  defaultConnectInfo
  { connectHost     = host config
  , connectPort     = fromInteger $ read $ port config
  , connectDatabase = dbs  config
  , connectUser     = user config
  , connectPassword = pass config
  }
