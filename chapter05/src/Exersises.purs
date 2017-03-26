module Exercises where

import Prelude
import Data.Maybe (Maybe(Nothing, Just))
import Data.Picture (Point(..), Shape(..))
import Math (pow, pi)

-- 5.5
-- 1
factorial :: Int -> Int
factorial 0 = 1
factorial n = n * factorial (n-1)
-- 2
binomialCoefficient :: Int -> Int -> Int
binomialCoefficient n k | k > n = 0
binomialCoefficient n 0 = 1
binomialCoefficient n k = binomialCoefficient (n-1) (k-1) +
                          binomialCoefficient (n-1) k

-- 5.9
type Address = { street :: String, city :: String }
type Person = { name :: String, address :: Address }
-- 1
sameCity :: Person -> Person -> Boolean
sameCity {address: { city: lhs}}  {address: { city: rhs}}
  | lhs == rhs = true
  | otherwise = false
-- 2
--sameCity :: forall r s t u.
--  { address :: { city :: String | s } | r } ->
--  { address :: { city :: String | t } | u } -> Boolean
-- 3
fromSingleton :: forall a. a -> Array a -> a
fromSingleton _ [x] = x
fromSingleton x _ = x

-- 5.13
-- 1
-- type Shape = {circle :: {radius :: Int}}


-- 5.14
-- 1
-- origin = Point {x:0.0,y:0.0}
-- circle = Circle origin 10.0
-- 2
scalePoint :: Point -> Point
scalePoint (Point {x, y}) = Point {x: 2.0 * x, y: 2.0 * y}

scaleShape :: Shape -> Shape
scaleShape (Circle p radius) = Circle (scalePoint p) (2.0 * radius)
scaleShape (Rectangle c w h) = Rectangle (scalePoint c) (2.0 * w) (2.0 * h)
scaleShape (Line start end) = Line (scalePoint start) (scalePoint end)
scaleShape (Text loc text) = Text (scalePoint loc) text
scaleShape (Clipped p w h s) = Clipped (scalePoint p) (2.0 * w) (2.0 * h) (scaleShape s)

-- 3
extractText :: Shape -> Maybe String
extractText (Text _ text) = Just text
extractText _ = Nothing

-- 5.17
-- 1
area :: Shape -> Number
area (Circle _ radius) = pow (pi * radius ) 2.0
area (Rectangle _ w h) = w * h
area _ = 0.0
-- 2
-- check inside Data/Pictures.purs
