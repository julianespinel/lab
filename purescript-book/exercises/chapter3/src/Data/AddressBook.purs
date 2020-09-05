module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, isJust)

type Address =
  { street :: String
  , city   :: String
  , state  :: String
  }

type Entry =
  { firstName :: String
  , lastName  :: String
  , address   :: Address
  }

type AddressBook = List Entry

showAddress :: Address -> String
showAddress addr = addr.street <> ", " <> addr.city <> ", " <> addr.state

showEntry :: Entry -> String
showEntry entry = entry.lastName <> ", " <> entry.firstName <> ": " <> showAddress entry.address

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = Cons

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = head <<< filter filterEntry
  where
  filterEntry :: Entry -> Boolean
  filterEntry entry = entry.firstName == firstName && entry.lastName == lastName

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street book = head $ filter filterByStreet book
  where filterByStreet :: Entry -> Boolean
        filterByStreet entry = entry.address.street == street

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName book = isJust $ findEntry firstName lastName book

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates book = nubBy hasSameName book
  where hasSameName :: Entry -> Entry -> Boolean
        hasSameName a b = a.firstName == b.firstName && a.lastName == b.lastName
