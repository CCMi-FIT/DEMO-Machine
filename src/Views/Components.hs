{-# LANGUAGE OverloadedStrings #-}

module Views.Components
  ( Cookies
  , DemoElement(..)
  , inputF
  , paramVal
  ) where

import qualified Data.Map.Lazy as M
import Data.Monoid ((<>))
import Data.Maybe (fromMaybe)
import Data.Text as T

import Text.Blaze.Internal (textValue)
import Text.Blaze.Html5 (toHtml, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

type Cookies = M.Map Text Text

data DemoElement
  = Initiator
  | Executor
  | TransactionKind
  | Product
  deriving (Show)

paramVal :: Cookies -> DemoElement -> Text
paramVal cookies el = let key = T.pack $ show el in
  if M.member key cookies
    then cookies M.! key
    else ""

inputF :: Cookies -> DemoElement -> Html
inputF cookies el = let name1 = T.pack $ show el in
  H.tr $
  do H.td (toHtml name1)
     H.td $ H.input ! A.name (textValue name1) ! A.value (textValue $ paramVal cookies el)
