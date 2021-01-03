{-# LANGUAGE FlexibleContexts, Safe #-}

{-|
Module      : Text.Parse.Trie
Description : A module to convert Strings to a trie-based parser.
Maintainer  : hapytexeu+gh@gmail.com
Stability   : experimental
Portability : POSIX

A module to convert Strings to a trie-based parser.
-}

module Text.Parse.Trie (
    parseTrie
  ) where

import Control.Applicative((<|>))
import Control.Arrow(first)

import Data.List(sortOn)
import Data.List.Group(groupWith)
import Data.List.NonEmpty(NonEmpty((:|)))

import Text.Parsec(ParsecT, Stream, parserReturn, parserZero)
import Text.Parsec.Char(char)

_chomp :: [a] -> [a]
_chomp [] = []
_chomp ~(_:xs) = xs

_neToList :: NonEmpty a -> [a]
_neToList (a :| as) = a : as

-- | Convert the given list of 2-tuples with a string to a trie-like parser.
parseTrie :: Stream s m Char
  => [(String, a)]  -- ^ The list of 2-tuples to convert to a trie-like parser.
  -> ParsecT s u m a  -- ^ The corresponding trie-like parser
parseTrie = _parseTrie . sortOn fst

_parseTrie :: Stream s m Char => [(String, a)] -> ParsecT s u m a
_parseTrie [] = parserZero
_parseTrie (("", x) : ss) = _parseTrieRem ss <|> parserReturn x
_parseTrie ss = _parseTrieRem ss

_parseTrieLeg :: Stream s m Char => Char -> NonEmpty (String, a) -> ParsecT s u m a
_parseTrieLeg c is = char c *> _parseTrie (map (first _chomp) (_neToList is))

_parseTrieRem :: Stream s m Char => [(String, a)] -> ParsecT s u m a
_parseTrieRem = foldr ((<|>) . uncurry _parseTrieLeg) parserZero . groupWith (head . fst)
