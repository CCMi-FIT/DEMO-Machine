{-# LANGUAGE OverloadedStrings #-}

module Views.Base
  ( MainPage(..)
  , MachinePage(..)
  , makePage
  ) where

import Data.Monoid ((<>))
import Data.Text as T
import qualified Data.Map as Map

import Text.Blaze.Internal (textValue)
import Text.Blaze.Html5 (toHtml, Html, (!))
import qualified Text.Blaze.Html5 as H
import qualified Text.Blaze.Html5.Attributes as A

import Config.Config (staticURL)

import Views.Components (Cookies)
import qualified Views.Home
import qualified Views.ModelDefinition
import qualified Views.Execution

data MainPage = Home | Machine deriving Eq
data MachinePage = NoMachine | ModelDefinition | Execution deriving Eq

class Eq p => Page p

instance Page MainPage
instance Page MachinePage

curCls :: Page p => p -> p -> H.AttributeValue
curCls p1 p2 = if p1 == p2 then "active" else ""

mainNav :: MainPage -> Html
mainNav current = do
  H.li ! A.class_ (curCls current $ Home) $
    H.a ! A.href "/" $ "About"
  H.li ! A.class_ (curCls current Machine) $
    H.a ! A.href "/modelDefinition" $ "The Machine"

machineNav :: MachinePage -> Html
machineNav NoMachine = mempty
machineNav current = do
  H.li ! A.class_ (curCls current ModelDefinition) $
    H.a ! A.href "/modelDefinition" $ "Model Definition"
  H.li ! A.class_ (curCls current Execution) $
    H.a ! A.href "/execution" $ "Execution"

contents :: MainPage -> MachinePage -> Cookies -> Html
contents Home _ _ = Views.Home.view
contents Machine NoMachine _ = Views.Home.view
contents Machine ModelDefinition cookies = Views.ModelDefinition.view cookies
contents Machine Execution cookies = Views.Execution.view cookies

makePage :: MainPage -> MachinePage -> Cookies -> Html
makePage mainPage machinePage cookies =
  H.docTypeHtml ! A.lang "en" $ do
    renderHead
    H.body $ do
      H.nav ! A.class_ "navbar navbar-inverse navbar-static-top" $ do
        H.div ! A.class_ "container" $ do
          H.div ! A.class_ "navbar-header" $ do
            H.button ! A.type_ "button" ! A.class_ "navbar-toggle collapsed"
              ! H.dataAttribute "data-toggle" "collapse"
              ! H.dataAttribute "data-target" "#navbar"
              ! H.dataAttribute "aria-expanded" "false"
              ! H.dataAttribute "aria-controls" "navbar"
              $ mempty
            H.a ! A.class_ "navbar-brand" ! A.href "#" $ "DEMO Machine v0.1"
          H.div ! A.id "navbar" ! A.class_ "collapse navbar-collapse" $ do
            H.ul ! A.class_ "nav navbar-nav" $ mainNav mainPage
      H.div ! A.class_ "container" $ do
        H.ul ! A.class_ "nav nav-pills" $ machineNav machinePage
        contents mainPage machinePage cookies
      renderFooter
      --renderAcknowledgement
    H.script ! A.src (textValue $ staticURL <> "js/vendor/jquery-3.1.1.min.js") $ mempty
    H.script ! A.src (textValue $ staticURL <> "js/vendor/bootstrap.min.js") $ mempty
    H.script ! A.src (textValue $ staticURL <> "js/main.js") $ mempty

renderHead :: Html
renderHead =
  H.head $
  do H.meta ! A.charset "utf-8"
     H.meta ! A.httpEquiv "X-UA-Compatible" ! A.content "IE=edge"
     H.meta ! A.name "viewport" ! A.content "width=device-width, initial-scale=1"
     H.title "DEMO Machine"
     H.link ! A.rel "stylesheet" ! A.href (textValue $ staticURL <> "css/bootstrap.min.css")
     H.link ! A.rel "stylesheet" ! A.href (textValue $ staticURL <> "css/sticky-footer-navbar.css")
     H.link ! A.rel "stylesheet" ! A.href (textValue $ staticURL <> "css/main.css")

renderFooter :: Html
renderFooter =
  H.footer ! A.class_ "footer" $
    H.div ! A.class_ "container" $ do
      H.div ! A.class_ "footer-section" $ do
        H.span "Author: "
        H.a ! A.href "mailto:robert.pergl@fit.cvut.cz" ! A.style "margin-right: 5px;" $ "Robert Pergl"
      H.div ! A.class_ "footer-section" $ do
        H.a ! A.href "http://ccmi.fit.cvut.cz/en" $
          H.img ! A.src (textValue $ staticURL <> "img/logo-ccmi.png") ! A.class_ "logo" ! A.alt "CCMi logo"
        H.a ! A.href "http://fit.cvut.cz/en" $
          H.img ! A.src (textValue $ staticURL <> "img/logo-fit.png") ! A.class_ "logo" ! A.alt "FIT logo"
      H.div ! A.class_ "footer-section" $ do
        H.span $ "Crafted with "
        H.a ! A.href "https://www.haskell.org/ghc/" ! A.class_ "colophon-text" $ "GHC"
        H.span $ " & "
        H.a ! A.href "http://haste-lang.org/" ! A.class_ "colophon-text" $ "Haste"
        H.span $ ", powered by "
        H.a ! A.href "http://hackage.haskell.org/package/scotty" ! A.class_ "colophon-text" $ "Scotty"
        H.img ! A.src (textValue $ staticURL <> "img/haskell.png") ! A.alt "Haskell logo" ! A.class_ "logo"

