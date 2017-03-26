module Exercises where

import Control.MonadZero (guard)
import Data.Array (length, filter, cons, uncons, (..))
import Data.Array.Partial (head, tail)
import Data.Foldable (foldl, foldMap)
import Data.Maybe (Maybe(Just, Nothing))
import Data.Monoid.Additive (Additive(Additive), runAdditive)
import Partial.Unsafe (unsafePartial)
import Prelude

-- 4.4
-- 1
isEven :: Int -> Boolean
isEven n =
  if n == 0
    then true
    else not isEven (n - 1)
-- 2
numbOfEven :: Array Int -> Int
numbOfEven arr =
  case uncons arr of
    Nothing -> 0
    Just a  | isEven a.head -> 1 + numbOfEven a.tail
            | otherwise -> numbOfEven a.tail
--2 for fun
numbOfEvenFmap :: Array Int -> Int
numbOfEvenFmap arr = runAdditive (foldMap count arr)
  where
    count :: Int -> Additive Int
    count a = Additive if isEven a then 1 else 0


-- 4.7
-- 1
toSquare :: Array Int -> Array Int
toSquare a = (\n -> n * n) <$> a
-- 2
removeNegative :: Array Int -> Array Int
removeNegative a = filter (\n -> n >= 0 ) a
-- 3
infix 4 filter as <$?>
-- 3 a
removeNegative' :: Array Int -> Array Int
removeNegative' a = (<$?>) (\n -> n > 0 ) a


-- 4.11
factors :: Int -> Array (Array Int)
factors n = do
  i <- 1 .. n
  j <- i .. n
  guard $ i * j == n
  pure [i, j]

factors' :: Int -> Array Int
factors' n = do
  j <- 1 .. n
  guard $ n `mod` j == 0
  [j]

-- 1
isPrime :: Int -> Boolean
isPrime n = (length (factors n)) /= 2
-- 2
cartesianProduct :: forall a. Array a -> Array a -> Array (Array a)
cartesianProduct xs ys = do
  i <- xs
  j <- ys
  pure [i, j]
-- 3
pythagoreanTriple :: Int -> Array (Array Int)
pythagoreanTriple n = do
  i <- (1 .. n)
  j <- (i .. n)
  k <- (j .. n)
  guard $ (i * i) + (j *j) == (k * k)
  pure [i, j, k]

-- 4
factorizations' :: Int -> Array (Array Int)
factorizations' n = cons [n] do
  factor <- factors' n
  guard $ factor > 1
  factorsOfNDivFactor <- factorizations' (n / factor)
  [cons factor factorsOfNDivFactor]

-- 4.15
-- 1
isArrayBool :: Array Boolean -> Boolean
isArrayBool = foldl (&&) true
-- 2

-- 3

-- count :: forall a. (a -> Boolean) -> Array a -> Int
-- count _ [] = 0
-- count p xs = if p (unsafePartial head xs)
--              then count p (unsafePartial tail xs) + 1
--              else count p (unsafePartial tail xs)

boolint :: Boolean -> Int
boolint true = 1
boolint _ = 0

counting :: forall a. (a -> Boolean) -> Array a -> Int
counting p acc =
  case uncons acc of
    Nothing -> 0
    Just { head, tail } -> boolint (p head) + counting p tail


-- 4
foldlReverse :: forall a. Array a -> Array a
foldlReverse = foldl (\acc cur -> [cur] <> acc) []
