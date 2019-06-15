module Data.AddressBook where

import Prelude

import Control.Plus (empty)
import Data.List (List, filter, head, (:))
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
showEntry entry =
    entry.lastName
    <> ", "
    <> entry.firstName
    <> ": "
    <> showAddress entry.address

showAddress :: Address -> String
showAddress address =
    address.street
    <> ", "
    <> address.city
    <> ", "
    <> address.state

emptyBook :: AddressBook
emptyBook = empty

insertEntry :: Entry -> AddressBook -> AddressBook
insertEntry = (:)

findEntry :: String -> String -> AddressBook -> Maybe Entry
findEntry firstName lastName = filter filterEntry >>> head
    where
        filterEntry :: Entry -> Boolean
        filterEntry entry =
            entry.firstName == firstName && entry.lastName == lastName

printEntry :: String -> String -> AddressBook -> Maybe String
printEntry firstName lastName addressBook
    = map showEntry (findEntry firstName lastName addressBook)
