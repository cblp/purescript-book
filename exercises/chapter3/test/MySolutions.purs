module Test.MySolutions where

import Prelude

import Data.Function (on)
import Data.List (filter, head, nubByEq, null)
import Data.Maybe (Maybe)
import Data.Tuple (Tuple(..))

import Data.AddressBook (Entry, AddressBook)

findEntryByStreet :: String -> AddressBook -> Maybe Entry
findEntryByStreet street =
  filter ((_.address.street) >>> (_ == street)) >>> head

isInBook :: String -> String -> AddressBook -> Boolean
isInBook firstName lastName =
  filter (\entry -> entry.firstName == firstName && entry.lastName == lastName)
    >>> null
    >>> not

removeDuplicates :: AddressBook -> AddressBook
removeDuplicates =
  nubByEq $ eq `on` \entry -> Tuple entry.firstName entry.lastName
