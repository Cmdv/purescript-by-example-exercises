module FileOperations where

import Prelude
import Data.Array (filter, concatMap, (:))
import Data.Foldable (any, foldl)
import Data.Maybe (Maybe(Nothing, Just))
import Data.Path (filename, size, root, isDirectory, Path, ls)

allFiles :: Path -> Array Path
allFiles root = root : concatMap allFiles (ls root)

allFiles' :: Path -> Array Path
allFiles' file = file : do
  child <- ls file
  allFiles' child
-- 4.16
-- 1
onlyFiles :: Path -> Array Path
onlyFiles = allFiles >>> filter (isDirectory >>> not)
-- 2
fileSmallest :: Path -> Path
fileSmallest = foldl pickSmallest root <<< allFiles'
  where
    pickSmallest :: Path -> Path -> Path
    pickSmallest acc curr =
      case size acc , size curr of
        Just sizeAcc , Just sizeCurr
          | sizeAcc > sizeCurr -> curr
        Nothing , _ -> curr
        _ , _ -> acc

-- 3
-- take a string -> traverse the file tree -> file || dir
-- case 1: if we find a directory look at files inside directory, to see
-- if child directory contains the file.
-- case 2: if the file matches the string return parent directory but if doesn't
-- give them Nothing
whereIs :: String -> Boolean
whereIs s = whereIsChild root
  where
    pathMatches :: Path -> Boolean
    pathMatches path = filename path == s
    -- finding if the children folder contains file
    whereIsChild :: Path -> Boolean
    -- forall a. (a -> Boolean ) -> Array a -> Boolean
    whereIsChild p = any traverseDown (ls p)
    traverseDown :: Path -> Boolean
    traverseDown a =
      if isDirectory a
      then  whereIsChild a
      else pathMatches a
