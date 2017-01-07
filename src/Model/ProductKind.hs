module Model.ProductKind where

data ProductKind = ProductKind
  { id :: String
  , fact :: String
  } deriving (Read, Show)
