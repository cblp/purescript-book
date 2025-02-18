module Test.Main where

import Prelude

import Data.Maybe (Maybe(..))
import Effect (Effect)
import Test.Unit (TestSuite, suite, test)
import Test.Unit.Assert as Assert
import Test.Unit.Main (runTest)

import Data.AddressBook (AddressBook, Entry, emptyBook, findEntry, insertEntry)
import Test.MySolutions (findEntryByStreet, isInBook, removeDuplicates)

john :: Entry
john =
  { firstName: "John"
  , lastName: "Smith"
  , address: { street: "123 Fake St.", city: "Faketown", state: "CA" }
  }

peggy :: Entry
peggy =
  { firstName: "Peggy"
  , lastName: "Hill"
  , address: { street: "84 Rainey St.", city: "Arlen", state: "TX" }
  }

ned :: Entry
ned =
  { firstName: "Ned"
  , lastName: "Flanders"
  , address:
      { street: "740 Evergreen Terrace", city: "Springfield", state: "USA" }
  }

book :: AddressBook
book = insertEntry john $ insertEntry peggy $ insertEntry ned emptyBook

otherJohn :: Entry
otherJohn =
  { firstName: "John"
  , lastName: "Smith"
  , address:
      { street: "678 Fake Rd.", city: "Fakeville", state: "NY" }
  }

bookWithDuplicate :: AddressBook
bookWithDuplicate = insertEntry john $ insertEntry otherJohn book

main :: Effect Unit
main =
  runTest do
    runChapterExamples
    suite "Exercise - findEntryByStreet" do
      test "Lookup existing" do
        Assert.equal (Just john) $ findEntryByStreet john.address.street book
      test "Lookup missing" do
        Assert.equal Nothing $ findEntryByStreet "456 Nothing St." book
    suite "Exercise - isInBook" do
      test "Check existing" do
        Assert.assert "Ned must be in book" $
          isInBook ned.firstName ned.lastName book
      test "Check missing" do
        Assert.assertFalse "unknown mustn't be in book" $
          isInBook "unknown" "person" book
      test "Exercise - removeDuplicates" do
        Assert.equal book $ removeDuplicates bookWithDuplicate

runChapterExamples :: TestSuite
runChapterExamples = do
  test "Todo for book maintainers - Add tests for chapter examples" do
    pure unit
  suite "findEntry" do
    test "Lookup existing" do
      Assert.equal (Just ned) $ findEntry ned.firstName ned.lastName book
    test "Lookup missing" do
      Assert.equal Nothing $ findEntry "unknown" "person" book
