module Chapter6 where

import Prelude
import Data.Array (cons)
import Data.Foldable (class Foldable, foldMap, foldl, foldr)
import Data.Monoid (class Monoid)

-- 6.4
-- 1
newtype Complex = Complex {real :: Number, imaginary :: Number}

instance showComplex :: Show Complex where
  show (Complex {real, imaginary}) = "Real: " <> show real <> ", Imaginary: " <> show imaginary

instance eqComplex :: Eq Complex where
  eq (Complex l) (Complex r) = l.real == r.real && l.imaginary == r.imaginary

-- 6.7
-- 1
data NonEmpty a = NonEmpty a (Array a)

instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
    eq (NonEmpty x xs) (NonEmpty y ys) = x == y && xs == ys

-- 2
instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
   append (NonEmpty x xs) (NonEmpty y ys) = NonEmpty x (xs <> [y] <> ys)

-- 3
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty x xs) = NonEmpty (f x) (map f xs)

-- 4
data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite x) (Finite y) = x == y
  eq _ _ = false

instance ordExtended ::  (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite x) (Finite y) = compare x y

-- 5
instance foldNonEmpty :: Foldable NonEmpty where
  foldr f acc (NonEmpty x xs) = foldr f acc (cons x xs)
  foldl f acc (NonEmpty x xs) = foldl f acc (cons x xs)
  foldMap f (NonEmpty x xs) = append (f x) (foldMap f xs)

-- 6
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f acc (OneMore x xs) = f x (foldr f acc xs)
  foldl f acc (OneMore x xs) = foldl f (f acc x) xs
  foldMap f (OneMore x xs) = append (f x) (foldMap f xs)

-- 6.11
-- 1/2
class Monoid m <= Action m a where
    act :: m -> a -> a

instance arrayAction :: Action m a => Action m (Array a) where
  act m xs = map (act m) xs

-- 3
newtype Self m = Self m

instance selfAppend :: Action m a => Action m (Self m) where
  act m (Self x) = Self (m <> x)

-- 4


