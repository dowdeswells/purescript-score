module AddressBook where

import Prelude

import Control.Plus (empty)
import Data.Foldable (lookup)
import Data.List (List(..), filter, head)
import Data.Maybe (Maybe)

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }


type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type AddressBook = List Entry



showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <>
                  entry.firstName <> ": " <>
                  showAddress entry.address

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <>
                   addr.city <> ", " <>
                   addr.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry e ab = Cons e ab

lookup :: String -> String -> AddressBook -> Maybe Entry
lookup fn ln ab = head $ filter (\e -> (_.firstName e) == fn && (_.lastName e) == ln) ab