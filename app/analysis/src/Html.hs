{-# LANGUAGE OverloadedStrings #-}

module Html ( genetateHtmlTable
            , genetateHtmlTableWithContents
            ) where

import           Control.Monad
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Csv                        (DefaultOrdered (headerOrder),
                                                  FromField (parseField),
                                                  FromNamedRecord (parseNamedRecord),
                                                  Header, ToField (toField),
                                                  ToNamedRecord (toNamedRecord),
                                                  (.:), (.=))
import qualified Data.Csv                        as Csv
import           Data.String.Utils
import qualified Data.Text                       as T
import qualified Data.Text.Encoding              as TE
import qualified Data.Text.Lazy                  as TL
import qualified Data.Text.Lazy.Encoding         as TEL
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import qualified Text.Blaze.Html.Renderer.String as H
import           Text.Blaze.Html5                (Html)
import qualified Text.Blaze.Html5                as H
import qualified Text.Blaze.Html5.Attributes     as A


import           Types

--------------------------------------------------------------------------------

-- | Generates Html data from a lazy bytestring of Csv data.
genetateHtmlTable :: LBS.ByteString -> Html
genetateHtmlTable bs =
  case Csv.decode Csv.NoHeader bs of
    Left err -> do
      H.h1 "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows ->
      H.table $ do
        H.thead $ makeRow (V.head rows)
        H.tbody $ forM_ (V.tail rows) makeRow

generateHtmlTable' :: LBS.ByteString -> Html
generateHtmlTable' bs = do
  case decodeCompanies bs of
    Left  err  -> do
      H.h1 "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows -> do
      let companies = V.toList rows
      H.table H.!
        A.class_ "sortable-theme-minimal" $ do
          H.thead $ makeHeaderRow
          H.tbody $ forM_ companies makeRow'

genetateHtmlTableWithContents :: LBS.ByteString -> Html
genetateHtmlTableWithContents bs =
  case Csv.decode Csv.NoHeader bs of
    Left err -> do
      H.h1 "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows -> do
      H.thead $ makeRow (V.head rows)
      H.tbody $ forM_   (V.tail rows) makeRow

makeRow :: Vector BS.ByteString -> Html
makeRow row =
  H.tr $ forM_ row $
    \column ->
      H.td $ H.toHtml . BS.unpack $ column

makeHeaderRow :: Html
makeHeaderRow =
   H.tr $ do
     H.th $ "#"
     H.th $ "Name"
     H.th $ "Type"
     H.th $ "Location"
     H.th $ "Area"

makeRow' :: Company -> Html
makeRow' row =
  H.tr $ do
    H.td $ H.toHtml $ cId row
    H.td $ H.toHtml $
      H.a
        H.! A.href  (H.stringValue $ cUrl row)
          $ H.toHtml $ cName row
    H.td $ H.toHtml $ cType row
    H.td $ H.toHtml $ if (T.null (cCountry row) && T.null (cCity row))
                      then ""
                      else T.concat [cCountry row, ", ", cCity row]
    H.td $ H.toHtml $ cArea row


makeTable :: FilePath -> IO (Either String String)
makeTable fp = do
  csvData <- LBS.readFile fp
  let htmlTable = genetateHtmlTable csvData

  case TEL.decodeUtf8' csvData of
    Left  err  -> do
      return $ Left $ "error decoding" ++ (show err)
    Right dat  -> do
      let htmlTable = generateHtmlTable' csvData
      return $ Right $ H.renderHtml htmlTable

makeViewTable :: FilePath -> IO ()
makeViewTable fp = do
  htmlTableE <- makeTable fp
  print htmlTableE

makeInsertTable :: FilePath -- ^ path to csv
                -> FilePath -- ^ path to orogonal html file
                -> FilePath -- ^ path to output html file
                -> String   -- ^ insertion pattern
                -> IO ()
makeInsertTable dfp ifp ofp ptrn = do
  htmlTableE <- makeTable dfp
  case htmlTableE of
    Left err ->
      print $ err
    Right htmlTable -> do
      htmlFile <- readFile ifp
      let newHtmlFile = replace ptrn htmlTable htmlFile
      writeFile ofp newHtmlFile

decodeCompanies :: LBS.ByteString -> Either String (V.Vector Company)
decodeCompanies = fmap snd . Csv.decodeByName

-- > :load src/Html.hs
-- > makeInsertTable "../../companies/active.csv" "../../external/index.html" "../../external/index_.html" "$table$"
