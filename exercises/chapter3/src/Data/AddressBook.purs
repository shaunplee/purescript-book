module Data.AddressBook where

import Prelude
import Control.Plus (empty)
import Data.List (List(..), filter, head, nubBy)
import Data.Maybe (Maybe, maybe)

type Address
  = { street :: String
    , city :: String
    , state :: String
    }

type Entry
  = { firstName :: String
    , lastName :: String
    , address :: Address
    }

type AddressBook
  = List Entry

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
findEntryByStreet street = head <<< filter ((==) street <<< _.address.street)

-- where
-- filterEntry :: Entry -> Boolean
-- filterEntry entry = entry.address.street == street
isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName ab = maybe false (const true) (findEntry firstName lastName ab)

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates = nubBy (\e1 e2 -> e1.firstName == e2.firstName && e1.lastName == e2.lastName)
