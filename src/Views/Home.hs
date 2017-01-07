{-# LANGUAGE OverloadedStrings #-}

module Views.Home
  ( homeView
  ) where

import Data.Monoid ((<>))
import Data.Text as T

import Text.Blaze.Internal (textValue)
import Text.Blaze.Html5 (toHtml, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Config.Config (staticURL)

renderHead :: Html
renderHead =
  H.head $
  do H.meta ! A.charset "utf-8"
     H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
     H.title "DEMO Machine"
     H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
     H.link ! A.rel "stylesheet" ! A.href (textValue $ staticURL <> "css/normalize.min.css")
     H.link ! A.rel "stylesheet" ! A.href (textValue $ staticURL <> "css/main.css")
     H.script ! A.src (textValue $ staticURL <> "js/vendor/jquery-3.1.1.min.js") $ mempty

renderBanner :: Html
renderBanner =
  H.div ! A.id "banner" $
  do H.a ! A.href "https://www.elixir-europe.org/" $
       H.img ! A.src (textValue $ staticURL <> "img/logo.png") ! A.id "logo" ! A.alt "Elixir logo"
     H.h1 ! A.class_ "title" $
       do _ <- "DEMO Machine"
          H.span ! A.class_ "version" $ " v0.1"

renderFooter :: Html
renderFooter =
  H.div ! A.id "footer" ! A.class_ "stripe" $
  H.table ! A.class_ "footer-table" $
  H.tbody $
  H.tr $
  do H.td $
       do H.h3 "Technical contacts"
          H.a ! A.href "mailto:robert.pergl@fit.cvut.cz" $ "Robert Pergl"
     H.td ! A.style "text-align: center; " $
       do H.h3 "Data stewardship action team"
          H.a ! A.href "http://elixir-czech.cz" $
            H.img ! A.src (textValue $ staticURL <> "img/logo-elixir-cz.jpg") ! A.class_ "logo" ! A.alt "ELIXIR-CZ logo"
          H.a ! A.href "http://ccmi.fit.cvut.cz/en" $
            H.img ! A.src (textValue $ staticURL <> "img/CCMi-logo.png") ! A.class_ "logo" ! A.alt "CCMi logo"
          H.a ! A.href "http://fit.cvut.cz/en" $
            H.img ! A.src (textValue $ staticURL <> "img/fit-logo.png") ! A.class_ "logo" ! A.alt "FIT logo"
          H.a ! A.href "http://www.dtls.nl/elixir-nl/elixir-nl-2/" $
            H.img ! A.src (textValue $ staticURL <> "img/logo-elixir-nl.png") ! A.class_ "logo" ! A.alt "ELIXIR-NL logo"
          H.a ! A.href "http://www.dtls.nl/" $
            H.img ! A.src (textValue $ staticURL <> "img/logo-dtl.png") ! A.class_ "logo" ! A.alt "DTL logo"
     H.td $
       do H.h3 "Links"
          H.a ! A.href "http://www.elixir-europe.org/" $ "ELIXIR Europe"
          H.br
          H.a ! A.href "http://www.elixir-europe.org/about/elixir-nodes" $ "ELIXIR Nodes"

renderAcknowledgement :: Html
renderAcknowledgement =
  H.div ! A.class_ "colophon-box" $
  do H.p ! A.class_ "colophon-line" $
       do H.span ! A.class_ "colophon-text" $ "Crafted with "
          H.a ! A.href "https://www.haskell.org/ghc/" ! A.class_ "colophon-text" $ "GHC"
          H.span ! A.class_ "colophon-text" $ " & "
          H.a ! A.href "http://haste-lang.org/" ! A.class_ "colophon-text" $ "Haste"
          H.span ! A.class_ "colophon-text" $ ", powered by "
          H.a ! A.href "https://www.spock.li/" ! A.class_ "colophon-text" $ "Spock"
          H.img ! A.src (textValue $ staticURL <> "img/haskell.png") ! A.alt "Haskell logo" ! A.class_ "logo"

inputF :: T.Text -> Html
inputF lbl = 
  H.tr $ do
    H.td (toHtml lbl)
    H.td H.input

transactionKindInput :: Html
transactionKindInput = do
  H.img ! A.src (textValue $ staticURL <> "img/transaction.png") ! A.style "margin: 10px 0;" ! A.alt "transaction"
  H.form $ do
    H.table $ 
      H.tbody $ do
        inputF "Product:"
        inputF "Transaction kind:"
        inputF "Initiator:"
        inputF "Executor:"
    H.input ! A.type_ "submit"


-- Pages
homeView :: Html
homeView =
  H.docTypeHtml ! A.class_ "no-js" ! A.lang "" $
  do renderHead
     H.body $
       H.div ! A.id "container" $ do
         renderBanner
         renderNav
         H.div ! A.class_ "inside-bordered" $ do
           transactionKindInput
         renderFooter
         renderAcknowledgement
         H.script ! A.src (textValue $ staticURL <> "js/main.js") $ mempty
  where
    renderNav = 
      H.ul ! A.class_ "nav" $
        H.li ! A.class_ "current" $ "Model definition"

