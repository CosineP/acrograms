{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.Char           (isAlpha, toLower)
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS

type AWord = String
type DictEntry = (AWord, Letters)
type Dictionary = Set DictEntry

type Anagram = MultiSet AWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- | Generate anagrams of the given word using the dictionary supplied.
-- | They also meet the requirements of the given acronym
anagrams :: Dictionary -> AWord -> AWord -> [AWord]
anagrams dict source acronym =
  map extractAnagram $ search narrowedDict sourceRefined acronym
  where
    narrowedDict = S.filter (\word -> any (`fitsAcronym` word) acronym) dict
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
    possibleWords = S.filter (canSpell remaining) dict
    -- THIS ONLY WORKS WHEN THE ACRONYM DOESN'T CONTAIN DUPLICATES
    -- For my pet use case that works but TODO: check for that
    (usableWords, newDict) = S.partition (fitsAcronym nextLetter) possibleWords
    -- We used to remove words from dictionary that we've used
    -- The acronym constraint makes that unnecessary
    allAnagrams = S.foldl go [] usableWords
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
  where goodWord "a" = True
        goodWord "i" = True
        --goodWord "o" = True -- bill wouldn't use this one
        goodWord w   = length w > 1
        --dictionary = "/usr/share/dict/words"
        dictionary = "10000.txt"
        toDict = S.fromList . map toDictEntry
        toDictEntry word = (word, wordLetters word)
