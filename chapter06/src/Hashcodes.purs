module Hashcodes where

import Prelude
import Data.NonEmpty as NE
import Data.Array (filter, group', length, nubBy, null)
import Data.Char (toCharCode)
import Data.Either (Either(..))
import Data.Foldable (foldl)
import Data.Function (on)
import Data.Maybe (Maybe(..))
import Data.String (toCharArray)
import Data.Tuple (Tuple(..))

newtype HashCode = HashCode Int

hashCode :: Int -> HashCode
hashCode h = HashCode (h `mod` 65535)

class Eq a <= Hashable a where
  hash :: a -> HashCode

instance showHashCode :: Show HashCode where
  show (HashCode h) = "(HashCode " <> show h <> ")"

derive instance eqHashCode :: Eq HashCode

derive instance ordHashCode :: Ord HashCode

combineHashes :: HashCode -> HashCode -> HashCode
combineHashes (HashCode h1) (HashCode h2) = hashCode (73 * h1 + 51 * h2)

hashEqual :: forall a. Hashable a => a -> a -> Boolean
hashEqual = eq `on` hash

instance hashInt :: Hashable Int where
  hash = hashCode

instance hashBoolean :: Hashable Boolean where
  hash false = hashCode 0
  hash true  = hashCode 1

instance hashChar :: Hashable Char where
  hash = hash <<< toCharCode

instance hashString :: Hashable String where
  hash = hash <<< toCharArray

instance hashArray :: Hashable a => Hashable (Array a) where
  hash = foldl combineHashes (hashCode 0) <<< map hash

instance hashMaybe :: Hashable a => Hashable (Maybe a) where
  hash Nothing = hashCode 0
  hash (Just a) = hashCode 1 `combineHashes` hash a

instance hashTuple :: (Hashable a, Hashable b) => Hashable (Tuple a b) where
  hash (Tuple a b) = hash a `combineHashes` hash b

instance hashEither :: (Hashable a, Hashable b) => Hashable (Either a b) where
  hash (Left a) = hashCode 0 `combineHashes` hash a
  hash (Right b) = hashCode 1 `combineHashes` hash b

-- 6.12
-- 2

hashDuplicates :: forall a. (Hashable a) => Array a -> Boolean
hashDuplicates a = length a /= length (nubBy hashEqual a)

-- just a bit of fun with adding Maybe type
-- hashDuplicates :: forall a. (Hashable a) => Array a -> Maybe (Array HashCode)
-- hashDuplicates a = if null hashish then Nothing else Just (map NE.head hashish)
--   where
--     hashish = filter (not <<< null <<< NE.tail) <<< group <<< sort $ map hash a
-- group' == group <<< sort

hashDuplicates' :: forall a. (Hashable a) => Array a -> Maybe (Array HashCode)
hashDuplicates' a = if null hashish then Nothing else Just (map NE.head hashish)
  where
    hashish = a
      # map hash
      # group'
      # filter (\x -> x # NE.tail # null # not)

newtype Hour = Hour Int

instance eqHour :: Eq Hour where
  eq (Hour n) (Hour m) = mod n 12 == mod m 12
