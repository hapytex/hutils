{-# LANGUAGE Safe #-}

{-|
Module      : Data.List.Maybe
Description : A module that performs list processing with 'Maybe' types.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module that works with lists of 'Maybe's.
-}

module Data.List.Maybe (
    MaybeList,
    concatMapMaybes
  ) where

import Data.Maybe(catMaybes)

-- | A type synonym for a list of 'Maybe' values.
type MaybeList a = [Maybe a]

-- | Concatenate the items wrapped in a 'Just' when we perform a mapping and concatenate the results of these mappings.
concatMapMaybes :: Foldable f
  => (a -> MaybeList b)  -- ^ The function that is used to perform a mapping from the items to a list of 'Maybe' values.
  -> f a  -- ^ The given 'Foldable' of items for which we perform a mapping.
  -> [b]  -- ^ As result a list of items that were wrapped in a 'Just' data constructor.
concatMapMaybes = concatMap . ( catMaybes .)
{-# SPECIALISE concatMapMaybes :: (a -> [Maybe b]) -> [a] -> [b] #-}
