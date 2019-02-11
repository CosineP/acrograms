{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.Char           (isAlpha, toLower)
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Data.Set            (Set)
import qualified Data.Set            as S

type AWord = String
type Dictionary = Set AWord

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
  | length wordsSoFar == length acronym && MS.null remaining = [wordsSoFar]
  -- Our final word didn't use up all our letters
  -- TODO: we could remove one function call by filtering these from possible words
  -- Don't think that'd help performance much but it could
  | length wordsSoFar == length acronym = []
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
    -- As we generate new branches, we remove words for which we have
    -- already created a branch: this ensures that independent branches
    -- will not generate identical sets of words.
    allAnagrams = fst $ foldl go ([], newDict) $ usableWords
    go (anagramsSoFar, d) word =
      (anagramsSoFar ++ expand acronym (MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d),
       S.delete word d)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters
    nextLetter = acronym !! length wordsSoFar

fitsAcronym :: Char -> AWord -> Bool
fitsAcronym letter word = (head word) == letter

wordLetters :: AWord -> Letters
wordLetters = MS.fromList


readDict :: IO Dictionary
readDict = (S.filter goodWord . S.fromList . lines) <$> readFile dictionary
  where goodWord "a" = True
        goodWord "i" = True
        --goodWord "O" = True -- bill wouldn't use this one
        goodWord w   = length w > 1
        -- TODO: if we use this dictionary again we have to lowercase it
        -- AND probably(?) remove apostrophes
        --dictionary = "/usr/share/dict/words"
        dictionary = "10000.txt"
