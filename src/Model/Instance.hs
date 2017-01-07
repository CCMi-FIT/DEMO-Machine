module Model.Instance where

data CActInst
  = Request Product
  | Promise Product
  | State Product
  | Accept Product

data CFact
  = Requested Product
  | Promised Product
  | Stated Product
  | Accepted Product
module ActorRole where

data ActorRole = ActorRole
  { id :: String
  , label :: String
  } deriving (Read, Show)
-- | Main entry point to the application.
module Main where

import qualified Text.Show.Pretty as Pr

data ActorRole = ActorRole {
    arId   :: String,
    arName :: String
} deriving (Read, Show)

data Product = Product {
    pId  :: String,
    pFact :: String
} deriving (Read, Show)

data TransactionKind = TransactionKind {
    tkId        :: String,
    tkName      :: String,
    tkInitiator :: ActorRole,
    tkExecutor  :: ActorRole,
    tkProduct   :: Product
} deriving (Read, Show)

type Description = String

data CAct = Request Description | Promise Description | State Description | Accept Description

p01 :: Product
p01 = Product {pId="P01", pFact="Membership is started"}
p02 :: Product
p02 = Product {pId="P02", pFact="the first fee of Membership is paid"}

ca01 :: ActorRole
ca01 = ActorRole {arId="CA01", arName="aspirant member"}
ca02 :: ActorRole
ca02 = ActorRole {arId="CA02", arName="payer"}
a01 :: ActorRole
a01 = ActorRole {arId="A01", arName="membership starter"}

actorRoles :: [ActorRole]
actorRoles = [ca01, ca02, a01]

t01 :: TransactionKind
t01 = TransactionKind {
    tkId = "T01",
    tkName = "membership start",
    tkInitiator = ca01,
    tkExecutor = a01,
    tkProduct = p01
}

t02 :: TransactionKind
t02 = TransactionKind {
    tkId = "T02",
    tkName = "membership payment",
    tkInitiator = a01,
    tkExecutor = ca02,
    tkProduct = p02
}

transactionKinds :: [TransactionKind]
transactionKinds = [t01, t02]

-- library :: ActorRole
-- library =


-- | The main entry point.
main :: IO ()
main = do
    putStrLn "DEMO execution engine prototype v1.0"
    putStr "Actor Roles: "
    putStrLn $ Pr.ppShow actorRoles
    putStr "Transaction kinds: "
    putStrLn $ Pr.ppShow transactionKindsmodule ProductKind where

data ProductKind = ProductKind
  { id :: String
  , fact :: String
  } deriving (Read, Show)
module TransactionKind where

import ActorRole (ActorRole)
import Product (Product)

data TransactionKind = TransactionKind
  { id :: String
  , name :: String
  , initiator :: ActorRole
  , executor :: ActorRole
  , product :: Product
  } deriving (Read, Show)
