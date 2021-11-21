module Test.MySolutions where

import Prelude

import Data.Foldable (sum)
import Data.Maybe (Maybe(..))
import Math (pi)

import ChapterExamples (Amp(..), Volt(..))
import Data.Picture (origin, Point, Shape(..))

factorial :: Int -> Int
factorial 1 = 1
factorial n
  | n < 1     = 1
  | otherwise = n * factorial (n - 1)

binomial :: Int -> Int -> Int
binomial n k
  | n <= 0 || k > n = 0
  | otherwise       = factorial n / factorial k / factorial (n - k)

pascal :: Int -> Int -> Int
pascal n k | n < 0 || k < 0 || k > n = 0
pascal _ 0                           = 1
pascal n k                           = pascal (n - 1) (k - 1) + pascal (n - 1) k

sameCity ::
  forall a c.
  { address :: { city :: String | c } | a } ->
  { address :: { city :: String | c } | a } ->
  Boolean
sameCity { address: { city: city1 } } { address: { city: city2 } } =
  city1 == city2

fromSingleton :: forall a. a -> Array a -> a
fromSingleton default arr =
  case arr of
    [x] -> x
    _   -> default

circleAtOrigin :: Shape
circleAtOrigin = Circle origin 10.0

doubleScaleAndCenter :: Shape -> Shape
doubleScaleAndCenter s =
  case s of
    Circle _ r        -> Circle origin (2.0 * r)
    Clipped ps b w h  -> Clipped (map doubleScaleAndCenter ps) b w h
    Line start end    ->
      Line (doublePoint $ start - mid) (doublePoint $ end - mid)
      where
        mid = middlePoint start end
    Rectangle _ w h -> Rectangle origin (2.0 * w) (2.0 * h)
    Text _ text     -> Text origin text

doublePoint :: Point -> Point
doublePoint {x, y} = {x: 2.0 * x, y: 2.0 * y}

middlePoint :: Point -> Point -> Point
middlePoint {x: x1, y: y1} {x: x2, y: y2} =
  {x: (x1 + x2) / 2.0, y: (y1 + y2) / 2.0}

shapeText :: Shape -> Maybe String
shapeText s =
  case s of
    Text _ text -> Just text
    _           -> Nothing

newtype Watt = Watt Number

calculateWattage :: Amp -> Volt -> Watt
calculateWattage (Amp a) (Volt v) = Watt $ a * v

area :: Shape -> Number
area s =
  case s of
    Circle _ r        -> pi * r * r
    Clipped ps _ _ _  -> sum $ map area ps
    Line _ _          -> 0.0
    Rectangle _ w h   -> w * h
    Text _ _          -> 0.0
