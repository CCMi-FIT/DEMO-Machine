{-# LANGUAGE OverloadedStrings #-}

module Main where

import Data.Text.Lazy (toStrict)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Spock as W
import Web.Spock.Config as WC
import Network.Wai.Middleware.Static as M
import Network.Wai.Middleware.RequestLogger

import Config.Server.Config (port)

import Views.Home (homeView)

type DemoApp = SpockM () () () ()

main :: IO ()
main = do
  spockCfg <- WC.defaultSpockCfg () WC.PCNoDatabase ()
  runSpock port (spock spockCfg app)

app :: DemoApp
app = do
  middleware M.static
  middleware logStdoutDev
  get root rootHandler

rootHandler :: ActionCtxT ctx (WebStateM () () ()) a
rootHandler = do
  sess <- readSession

  html $ toStrict $ renderHtml homeView
