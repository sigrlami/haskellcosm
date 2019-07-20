{-# LANGUAGE OverloadedStrings #-}

module Html ( genetateHtmlTable
            , genetateHtmlTableWithContents
            ) where

import           Control.Monad
import qualified Data.ByteString.Char8           as BS
import qualified Data.ByteString.Lazy.Char8      as LBS
import           Data.Csv
import           Data.Vector                     (Vector)
import qualified Data.Vector                     as V
import qualified Text.Blaze.Html.Renderer.String as H
import           Text.Blaze.Html5                (Html)
import qualified Text.Blaze.Html5                as H

--------------------------------------------------------------------------------

-- | Generates Html data from a lazy bytestring of Csv data.
genetateHtmlTable :: LBS.ByteString -> Html
genetateHtmlTable bs =
  case decode NoHeader bs of
    Left err -> do
      H.h1 "Error parsing CSV file!"
      H.p $ H.toHtml err
    Right rows ->
      H.table $ do
        H.thead $ makeRow (V.head rows)
        H.tbody $ forM_ (V.tail rows) makeRow

genetateHtmlTableWithContents :: LBS.ByteString -> Html
genetateHtmlTableWithContents bs =
  case decode NoHeader bs of
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

makeTable :: FilePath -> IO ()
makeTable fp = do
  csvData <- LBS.readFile fp
  let htmlTable = genetateHtmlTable csvData

  print $ H.renderHtml htmlTable
