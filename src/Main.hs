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

main :: IO ()
main = do
  spockCfg <- WC.defaultSpockCfg () WC.PCNoDatabase ()
  runSpock port (spock spockCfg app)

app :: SpockM () () () ()
app = do
  middleware M.static
  middleware logStdoutDev
  get root rootHandler

rootHandler :: ActionCtxT ctx (WebStateM () () ()) a
rootHandler = html $ toStrict $ renderHtml homeView
