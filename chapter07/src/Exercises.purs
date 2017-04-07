module Chapter7 where

import Prelude
import Control.Apply (lift2)
import Data.Maybe (Maybe(..))


-- 7.8
-- 1
maybeAdd :: forall n. (Semiring n) => Maybe n -> Maybe n -> Maybe n
maybeAdd = lift2 (+)

maybeMultiply :: forall n. (Semiring n) => Maybe n -> Maybe n -> Maybe n
maybeMultiply = lift2 (*)

maybeSubtract :: forall n. (Ring n) => Maybe n -> Maybe n -> Maybe n
maybeSubtract = lift2 (-)

maybeDivide :: forall n. (EuclideanRing n) => Maybe n -> Maybe n -> Maybe n
maybeDivide = lift2 (/)

-- 3
combineMaybe :: forall a f. Applicative f => Maybe (f a) -> f (Maybe a)
combineMaybe Nothing = pure Nothing
combineMaybe (Just a) = pure <$> a
