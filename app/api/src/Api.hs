{-# LANGUAGE AllowAmbiguousTypes        #-}
{-# LANGUAGE BlockArguments             #-}
{-# LANGUAGE DataKinds                  #-}
{-# LANGUAGE DeriveAnyClass             #-}
{-# LANGUAGE DeriveGeneric              #-}
{-# LANGUAGE FlexibleContexts           #-}
{-# LANGUAGE FlexibleInstances          #-}
{-# LANGUAGE FunctionalDependencies     #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE MultiParamTypeClasses      #-}
{-# LANGUAGE OverloadedStrings          #-}
{-# LANGUAGE PolyKinds                  #-}
{-# LANGUAGE RecordWildCards            #-}
{-# LANGUAGE ScopedTypeVariables        #-}
{-# LANGUAGE TemplateHaskell            #-}
{-# LANGUAGE TypeFamilies               #-}
{-# LANGUAGE TypeOperators              #-}

module Api ( restAPIvCombined
           , serverCombined
           ) where

import qualified Aws
import qualified Aws.Core                              as Aws
import qualified Aws.S3                                as S3
import           Codec.Archive.Zip
import           Control.Applicative
import           Control.Concurrent
import qualified Control.Exception                     as E
import           Control.Lens
import           Control.Monad
import           Control.Monad.IO.Class
import           Control.Monad.Random
import           Control.Monad.Trans.Resource          (runResourceT)
import           Crypto.Hash                           (Digest, SHA256, hash)
import           Crypto.MAC.HMAC                       (HMAC (..), hmac)
import           Data.Aeson
import qualified Data.ByteString                       as BS
import qualified Data.ByteString.Base64                as BS64 (decodeLenient)
import qualified Data.ByteString.Char8                 as BSC
import qualified Data.ByteString.Lazy                  as BSL
import           Data.Char
import           Data.Default
import           Data.Elocrypt
import           Data.Maybe
import           Data.Monoid
import           Data.Proxy
import           Data.Text                             as T
import           Data.Text.Encoding                    as TE
import           Data.Time
import           Data.Time.Clock
import           Data.Time.LocalTime
import qualified Data.UUID                             as Uuid
import qualified Data.UUID.V4                          as Uuid
import           Data.Word                             (Word8)
import           Database.PostgreSQL.Simple
import           GHC.Generics
import           Jose.Jws
import           Network.HTTP.Client.MultipartFormData
import           Network.HTTP.Conduit                  (RequestBody (..),
                                                        newManager,
                                                        tlsManagerSettings)
import qualified Network.Socket                        as NSocket
import           Network.Wai
import           Network.Wai.Handler.Warp
import           Safe
import           Servant                               hiding (BadPassword,
                                                        NoSuchUser)
import           Servant.API
import           Servant.API.BasicAuth
import           Servant.API.Experimental.Auth         (AuthProtect)
import           Servant.Auth                          hiding (NoSuchUser)
import           Servant.Auth.Server
import           Servant.Auth.Server.SetCookieOrphan   ()
import           Servant.HTML.Blaze
import           Servant.Multipart
import           Servant.Server                        hiding (BadPassword,
                                                        NoSuchUser)
import           Servant.Swagger
import           Servant.Utils.StaticFiles
import           System.Directory
import           System.Environment                    (getEnv)
import           System.FilePath.Posix
import           System.IO
import           System.IO.Error                       hiding (catch)
import           System.Posix.Files
import           System.Process
import           System.Random

-----------------------------------------------------------------------------

restAPIvCombined = undefined
serverCombined   = undefined
