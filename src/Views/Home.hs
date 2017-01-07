{-# LANGUAGE OverloadedStrings #-}

module Views.Home
  ( view
  ) where

--import Data.Monoid ((<>))
--import Data.Text as T

--import Text.Blaze.Internal (textValue)
import Text.Blaze.Html5 (Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

--import Config.Config (staticURL)

view :: Html
view = H.p "Design and Engineering Method for Organisations Machine Haskell reference implementation"

