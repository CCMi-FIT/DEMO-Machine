{-# LANGUAGE OverloadedStrings #-}

module Views.Execution
  ( view
  ) where

import Data.Monoid ((<>))
--import Data.Text as T

import Text.Blaze.Internal (textValue)
import Text.Blaze.Html5 (Html, toHtml, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Config.Config (staticURL)

import Views.Components (Cookies, DemoElement(..), inputF, paramVal)

view :: Cookies -> Html
view cookies = do
  H.p "exectution"
