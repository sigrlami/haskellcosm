{-# LANGUAGE CPP                 #-}
{-# LANGUAGE ConstraintKinds     #-}
{-# LANGUAGE FlexibleContexts    #-}
{-# LANGUAGE OverloadedStrings   #-}
{-# LANGUAGE RecordWildCards     #-}
{-# LANGUAGE RecursiveDo         #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TemplateHaskell     #-}
{-# LANGUAGE TupleSections       #-}
{-# LANGUAGE TypeFamilies        #-}

import           Control.Lens           hiding ((.=))
import           Control.Monad
import           Data.Aeson
import           Data.Bool (bool)
import           Data.DList (DList)
import qualified Data.DList as DL
import           Data.Map (Map)
import qualified Data.Map as M
import           Data.Maybe
import           Data.Monoid
import qualified Data.Set as S
import           Data.Text (Text)
import qualified Data.Text as T
import           Data.Time
import qualified GHCJS.DOM.Element as DOM
import           Reflex
import           Reflex.Dom
import           Reflex.Dom.Contrib.Utils


menu :: MonadWidget t m => Dynamic t (Maybe Player) -> m ()
menu myName = do
    divClass "ui blue inverted attached borderless menu" $ do
      elClass "span" "item active" $ text "HaskellCosm"
      divClass "right menu" $ do
        elAttr "a" ("class" =: "item" <> "href" =: "/logout")
    return ()

runApp :: MonadWidget t m => m ()
runApp = undefined

main :: IO ()
main = mainWidget runApp
