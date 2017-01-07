{-# LANGUAGE OverloadedStrings #-}

module Views.ModelDefinition
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
  H.div ! A.class_ "model-def-label" $ toHtml $ paramVal cookies Initiator
  H.div ! A.class_ "model-def-label" $ toHtml $ (paramVal cookies TransactionKind) <> " -> " <> (paramVal cookies Product)
  H.div ! A.class_ "model-def-label" $ toHtml $ paramVal cookies Executor
  H.div $ H.img ! A.src (textValue $ staticURL <> "img/transaction.png") ! A.style "margin: 10px 0;" ! A.alt "transaction"
  H.div $ H.form ! A.method "post" $ do
    H.table $ 
      H.tbody $ do
        inputF cookies Product 
        inputF cookies TransactionKind 
        inputF cookies Initiator 
        inputF cookies Executor 
    H.input ! A.type_ "submit"
