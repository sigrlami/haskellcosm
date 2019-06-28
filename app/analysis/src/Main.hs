{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Etl.Internal.Core as E
import           Etl.Julius
import qualified RTable.Core       as T
import qualified RTable.Data.CSV   as C

--------------------------------------------------------------------------------

src_DBTab_MData :: RTableMData
src_DBTab_MData =
  createRTableMData
    ( "active"  -- table name
    , [ ("N"          , Integer)
      , ("Name"       , Varchar)
      , ("Url"        , Varchar)
      , ("Type"       , Varchar)
      , ("Country"    , Varchar)
      , ("City"       , Varchar)
      , ("Area"       , Varchar)
      , ("Description", Varchar)
      , ("Comment"    , Varchar)
      ]
    )
    ["Name", "TABLE_NAME"] -- primary key
    []                     -- (alternative) unique keys

main :: IO ()
main = do
  -- read source csv file
  srcCSV <- readCSV "../../companies/active.csv"
  let src_DBTab = toRTable src_DBTab_MData srcCSV  -- turn source csv to an RTable

  return $ ()
