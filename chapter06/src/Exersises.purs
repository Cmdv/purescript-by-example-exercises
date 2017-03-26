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

-- 2
data NonEmpty a = NonEmpty a (Array a)

instance semigroupNonEmpty :: Semigroup (NonEmpty a) where
   append (NonEmpty a arr) (NonEmpty b arr') = NonEmpty a (append arr (append [b] arr'))

-- 3
instance functorNonEmpty :: Functor NonEmpty where
  map f (NonEmpty a arr) = NonEmpty (f a) (map f arr)

-- 4
instance foldNonEmpty :: Foldable NonEmpty where
  foldr f acc (NonEmpty a arr) = foldr f acc (cons a arr)
  foldl f acc (NonEmpty a arr) = foldl f acc (cons a arr)
  foldMap f (NonEmpty a arr) = append (f a) (foldMap f arr)


-- 6.7
-- 1
instance eqNonEmpty :: (Eq a) => Eq (NonEmpty a) where
    eq (NonEmpty a arr) (NonEmpty a' arr') = a == a' && arr == arr'

--2
data Extended a = Finite a | Infinite

instance eqExtended :: (Eq a) => Eq (Extended a) where
  eq Infinite Infinite = true
  eq (Finite a) (Finite a') = a == a'
  eq _ _ = false

instance ordExtended ::  (Ord a) => Ord (Extended a) where
  compare Infinite Infinite = EQ
  compare Infinite _ = GT
  compare _ Infinite = LT
  compare (Finite a) (Finite a') = compare a a'

-- 3
data OneMore f a = OneMore a (f a)

instance foldableOneMore :: Foldable f => Foldable (OneMore f) where
  foldr f acc (OneMore a rest) = f a (foldr f acc rest)
  foldl f acc (OneMore a rest) = foldl f (f acc a) rest
  foldMap f (OneMore a rest) = append (f a) (foldMap f rest)


-- 6.10
-- 1/2
class Monoid m <= Action m a where
    act :: m -> a -> a

instance arrayAction :: Action m a => Action m (Array a) where
  act m array = map (act m) array

-- 3
newtype Self m = Self m

instance selfAppend :: Action m a => Action m (Self m) where
  act m (Self x) = Self (m <> x)

-- 4


