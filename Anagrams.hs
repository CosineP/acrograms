{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.List           (partition)
import           Data.Char           (isAlpha, toLower)
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS

type AWord = String
type Dictionary = [AWord]

type Anagram = MultiSet AWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

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
    go anagramsSoFar word =
      anagramsSoFar ++ expand (tail acronym) (MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, newDict)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters
    canAcronym = canSpell remaining acronym
    nextLetter = head acronym

fitsAcronym :: Char -> AWord -> Bool
fitsAcronym letter word = (head word) == letter

wordLetters :: AWord -> Letters
wordLetters = MS.fromList

readDict :: IO Dictionary
readDict = (filter goodWord . lines) <$> readFile dictionary
  where goodWord "a" = True
        goodWord "i" = True
        --goodWord "o" = True -- bill wouldn't use this one
        goodWord w   = length w > 1
        --dictionary = "/usr/share/dict/words"
        dictionary = "10000.txt"
