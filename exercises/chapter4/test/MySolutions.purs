module Test.MySolutions where

import Prelude

import Control.Alternative (guard)
import Data.Array (catMaybes, filter, foldl, head, length, null, tail, uncons, (..), (:))
import Data.Int (even, rem, toNumber, floor)
import Data.Maybe (Maybe(..), fromMaybe)
import Data.Path (Path, filename, isDirectory, ls, size)
import Math (sqrt)
import Test.Examples (allFiles, factors)

isEven :: Int -> Boolean
isEven 0 = true
isEven 1 = false
isEven n
  | n > 0 = isEven (n - 2)
  | otherwise = isEven (n + 2)

countEven :: Array Int -> Int
countEven arr =
  if null arr then
    0
  else
    let
      x = fromMaybe 0 $ head arr
      xs = fromMaybe [] $ tail arr
      c
        | even x = 1
        | otherwise = 0
    in
      c + countEven xs

squared :: Array Number -> Array Number
squared = map (\x -> x * x)

keepNonNegative :: Array Number -> Array Number
keepNonNegative = filter (_ >= 0.0)

infix 5 filter as <$?>

keepNonNegativeRewrite :: Array Number -> Array Number
keepNonNegativeRewrite = ((_ >= 0.0) <$?> _)

isPrime :: Int -> Boolean
isPrime n = n > 1 && length (factors n) == 1

cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  x <- xs
  y <- ys
  pure [ x, y ]

triples :: Int -> Array (Array Int)
triples n = do
  a <- 1 .. n
  b <- a .. n
  c <- b .. n
  guard $ a * a + b * b == c * c
  pure [ a, b, c ]

primeFactors :: Int -> Array Int
primeFactors n
  | n > 1 = p : ps
      where
      n' = floor $ sqrt $ toNumber n
      p
        | n' >= 2 =
            fromMaybe n
              $ head
              $ filter (\d -> n `rem` d == 0)
              $ 2 .. n'
        | otherwise = n
      ps = primeFactors $ n `div` p
  | otherwise = []

allTrue :: Array Boolean -> Boolean
allTrue = foldl (&&) true

fibTailRec :: Int -> Int
fibTailRec = go 0 1
  where
  go a _ 0 = a
  go _ b 1 = b
  go a b n = go b (a + b) (n - 1)

reverse :: forall a. Array a -> Array a
reverse = foldl (\xs x -> [ x ] <> xs) []

onlyFiles :: Path -> Array Path
onlyFiles file = do
  child <- ls file
  if isDirectory child then
    onlyFiles child
  else
    pure child

whereIs :: Path -> String -> Maybe Path
whereIs dir = head <<< whereAll dir

whereAll :: Path -> String -> Array Path
whereAll dir name = do
  child <- ls dir
  if isDirectory child then
    whereAll child name
  else if filename child == filename dir <> name then
    pure dir
  else
    []

type Acc = { smallest :: Path, largest :: Path }

largestSmallest :: Path -> Array Path
largestSmallest = onlyFiles >>> uncons >>> foldMinMax
  where

  foldMinMax :: Maybe { head :: Path, tail :: Array Path } -> Array Path
  foldMinMax Nothing = []
  foldMinMax (Just { head, tail }) =
    toArray $ foldl getMinMax { smallest: head, largest: head } tail

  getMinMax :: Acc -> Path -> Acc
  getMinMax { smallest, largest } file =
    { smallest: minMaybeBy size file smallest
    , largest: maxMaybeBy size file largest
    }

  toArray :: Acc -> Array Path
  toArray { smallest, largest }
    | filename smallest == filename largest = [ smallest ]
    | otherwise = [ largest, smallest ]

minMaybeBy :: forall a k. Ord k => (a -> k) -> a -> a -> a
minMaybeBy k x y = if k x <= k y then x else y

maxMaybeBy :: forall a k. Ord k => (a -> k) -> a -> a -> a
maxMaybeBy k x y = if k x >= k y then x else y
