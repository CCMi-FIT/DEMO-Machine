module Model.TransactionKind where

import ActorRole (ActorRole)
import Product (Product)

data TransactionKind = TransactionKind
  { id :: String
  , name :: String
  , initiator :: ActorRole
  , executor :: ActorRole
  , product :: Product
  } deriving (Read, Show)
