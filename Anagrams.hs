{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.List           (partition)
import           Data.Char           (isAlpha, toLower)
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Debug.Trace         (trace, traceShow)

type AWord = String
type DictEntry = (AWord, Letters)
type Dictionary = [DictEntry]

type Anagram = MultiSet AWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- Convenient for debugging
traceThis :: Show a => a -> a
traceThis thing = traceShow thing thing

-- | Generate anagrams of the given word using the dictionary supplied.
-- | They also meet the requirements of the given acronym
anagrams :: Dictionary -> AWord -> AWord -> [AWord]
anagrams dict source acronym =
  map extractAnagram $ search narrowedDict sourceRefined acronym
  where
    narrowedDict = filter (\word -> any (`fitsAcronym` word) acronym) dict
    sourceRefined = filter isAlpha . map toLower $ source

search :: Dictionary -> AWord -> AWord -> [Anagram]
search dict source acronym = expand acronym initialState
  where initialState = (MS.empty, wordLetters source, dict)

extractAnagram :: Anagram -> AWord
extractAnagram = unwords . MS.toList

expand :: AWord -> SearchState -> [Anagram]
expand acronym (wordsSoFar, remaining, dict)
  -- We have just done the final word
  | 0 == length acronym && MS.null remaining = [wordsSoFar]
  -- Our final word didn't use up all our letters
  -- TODO: we could remove one function call by filtering these from possible words
  -- Don't think that'd help performance much but it could
  | 0 == length acronym = []
  -- Do we at least have one of each letter in the acronym?
  | not canAcronym = []
  -- Just for debug tracing
  | 3 <= length acronym = trace (show wordsSoFar ++ (show $ length dict)) allAnagrams
  -- We have work to do my friends
  | otherwise = allAnagrams
  where
    -- The distinction from possible and usable is this:
    -- possibleWords might be used down the line
    -- usableWords can be used in this spot in the acronym
    -- combining the two would lead to the first letter of the acronym
    -- stripping all non-acronymous words
    possibleWords = filter (canSpell remaining) dict
    -- THIS ONLY WORKS WHEN THE ACRONYM DOESN'T CONTAIN DUPLICATES
    -- For my pet use case that works but TODO: check for that
    (usableWords, newDict) = partition (fitsAcronym nextLetter) possibleWords
    -- We used to remove words from dictionary that we've used
    -- The acronym constraint makes that unnecessary
    allAnagrams = foldl go [] usableWords
    --allAnagrams = foldl go [] (take 20 usableWords) -- Duplicate of above for profiling
    go anagramsSoFar (word, set) =
      anagramsSoFar ++ expand (tail acronym) (MS.insert word wordsSoFar,
        remaining `MS.difference` set, newDict)
    canSpell letters (_, entryLetters) = entryLetters `MS.isSubsetOf` letters
    canAcronym = wordLetters acronym `MS.isSubsetOf` remaining
    nextLetter = head acronym

fitsAcronym :: Char -> DictEntry -> Bool
fitsAcronym letter (word, _) = head word == letter

wordLetters :: AWord -> Letters
wordLetters = MS.fromList

readDict :: IO Dictionary
readDict = toDict . (filter goodWord . lines) <$> readFile dictionary
  where
    goodWord (c:rest)
      | (c:rest) `elem` certain = True
      -- Whether a word is in "certain"
      | any (\x -> head x == c) certain = False
      -- a and i but no other one-letter words
      | (c:rest) == "a" = True
      | (c:rest) == "i" = True
      | length rest > 0 = True
      | otherwise = False
    goodWord [] = False
    -- Bill Wurtz told us it contains "of", meaning the o IS of
    -- TODO: in general case, remove this
    --certain = ["of"]
    -- Adding a few certains of early words can make a fast but
    -- realistic profile case
    certain = ["of", "well", "into"]
    -- Use comments here to select a dictionary to use
    --dictionary = "/usr/share/dict/words"
    --dictionary = "10000.txt"
    -- In this dictionary I've removed words we've already computed
    dictionary = "unknown.txt"
    toDict = map toDictEntry
    toDictEntry word = (word, wordLetters word)

