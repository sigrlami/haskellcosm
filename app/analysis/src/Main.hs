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
  srcCSV <- C.readCSV "../../companies/active.csv"
  let src_DBTab = toRTable src_DBTab_MData srcCSV  -- turn source csv to an RTable

  -- Let's find all companies that use Haskell for Internal products
  resultRTab <- runJulius $ julExpr "IP" $ toRTable src_DBTab_MData srcCSV
  printfRTable
    ( genRTupleFormat
        ["Name", "Type", "Country", "City", "Area", "Description"]
        genDefaultColFormatMap
    ) $ resultRTab


  -- write data to file if needed
  -- writeCSV "result.csv" $ fromRTable result_tab_MData resultRTab

  return $ ()


julExpr srch rtab =
  EtlMapStart
    :-> (EtlR $
           ROpStart
           :.  (Filter (From $ Tab rtab) $
                 FilterBy (\t -> case instrRText (RText srch) (t <!> "active") of
                                   Just p  -> True
                                   Nothing -> False
                                 &&
                                   (t <!> "Type") == (RText srch)
                          )
               )
        )
