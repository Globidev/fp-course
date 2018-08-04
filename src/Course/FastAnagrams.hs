{-# LANGUAGE NoImplicitPrelude #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Course.FastAnagrams where

import Course.Core
import Course.List
import Course.Functor
import qualified Data.Set as S

-- Return all anagrams of the given string
-- that appear in the given dictionary file.
fastAnagrams ::
  Chars
  -> FilePath
  -> IO (List Chars)
fastAnagrams s fp =
  let allAnagrams = permutations s
      anagramsSet = S.fromList (toNoCaseList allAnagrams)
      dict = S.fromList . toNoCaseList . lines <$> readFile fp
  in fromNoCaseSet . S.intersection anagramsSet <$> dict

  where
    toNoCaseList = hlist . (map NoCaseString)
    fromNoCaseSet = map ncString . listh . S.toList

instance Ord NoCaseString where
  compare = compare `on` map toLower . ncString

newtype NoCaseString =
  NoCaseString {
    ncString :: Chars
  }

instance Eq NoCaseString where
  (==) = (==) `on` map toLower . ncString

instance Show NoCaseString where
  show = show . ncString
