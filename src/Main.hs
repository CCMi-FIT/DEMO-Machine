{-# LANGUAGE OverloadedStrings #-}

module Main where

import qualified Data.Map as Map
import qualified Data.Text.Lazy
import qualified Data.Text
import qualified Data.Text.Internal.Lazy
--import Control.Monad.Trans (liftIO)
import Text.Blaze.Html.Renderer.Text (renderHtml)
import Web.Scotty
import Web.Scotty.Cookie (setSimpleCookie, getCookies)
import qualified Network.Wai.Middleware.Static as M
import Network.Wai.Middleware.RequestLogger (logStdoutDev)

import Config.Server.Config (port)

import Views.Components (DemoElement(..))
import qualified Views.Base as V

main :: IO ()
main = scotty port $ do
  middleware M.static
  middleware logStdoutDev
  get "/" $ html $ renderHtml $ V.makePage V.Home V.NoMachine (Map.fromList [])
  get "/modelDefinition" $ getCookies >>= (html . renderHtml . V.makePage V.Machine V.ModelDefinition)
  post "/modelDefinition" $ defineModel
  get "/execution" $ getCookies >>= (html . renderHtml . V.makePage V.Machine V.Execution)

e2tl :: DemoElement -> Data.Text.Internal.Lazy.Text
e2tl el = Data.Text.Lazy.pack $ show el

e2t :: DemoElement -> Data.Text.Text
e2t el = Data.Text.pack $ show el

defineModel :: ActionM ()
defineModel = do
  param (e2tl Product) >>= setSimpleCookie (e2t Product)
  param (e2tl TransactionKind) >>= setSimpleCookie (e2t TransactionKind)
  param (e2tl Initiator) >>= setSimpleCookie (e2t Initiator)
  param (e2tl Executor) >>= setSimpleCookie (e2t Executor)
  redirect "/modelDefinition"




