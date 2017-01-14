{-# LANGUAGE OverloadedStrings #-}

module Main where

import           Prelude
--import           Haste
--import           Haste.JSString (pack)

import           JQuery

main :: IO ()
main = ready $ do
  let cookie = mkCookie "testCookie" "testVal"
  setCookie cookie
  res <- getCookie "testCookie"
  dumptIO $ show res
