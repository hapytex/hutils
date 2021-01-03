{-# LANGUAGE Safe #-}

{-|
Module      : Data.List.Group
Description : A module to group elements of a list together in sequences.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that defines functions that groups a list of elements into subsequences.
-}

module Data.List.Group (
  -- * Grouping with the key
    groupWith, groupWith'
  ) where

import Data.List.NonEmpty(NonEmpty((:|)))

-- | Group the given list of items to a list of 'NonEmpty' 2-tuples with the
-- "key" and the value for a given mapping function and equavalence relation.
groupWith'
  :: (b -> b -> Bool)  -- ^ The given equivalence relation.
  -> (a -> b)  -- ^ The given mapping function.
  -> [a]  -- ^ The list of items to group into non-empty sequences.
  -> [(b, NonEmpty a)]  -- ^ The list of groups.
groupWith' eq f = go
    where go [] = []
          go ~(x:xs) = (fx, x :| ys) : go zs
              where ~(ys, zs) = span (eq fx . f) xs
                    fx = f x

-- | Group the given list of items to a list of 'NonEmpty' 2-tuples with the
-- "key" and the value for a given mapping function.
groupWith :: Eq b
  => (a -> b)  -- ^ The given mapping function.
  -> [a]  -- ^ The list of items to group into non-empty sequences.
  -> [(b, NonEmpty a)]  -- ^ The list of groups.
groupWith = groupWith' (==)
