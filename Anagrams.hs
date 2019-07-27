{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.Char           (isAlpha, toLower)
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           GHC.Exts            (groupWith)
import           Data.List           (elemIndex, insertBy)
import           Debug.Trace         (trace, traceShow)

type AWord = String
type DictEntry = (AWord, Letters)
type DictLetter = [DictEntry]
type Dictionary = [DictLetter]

type Anagram = MultiSet AWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- Convenient for debugging
traceThis :: Show a => a -> a
traceThis thing = traceShow thing thing

-- | Generate anagrams of the given word using the dictionary supplied.
-- | They also meet the requirements of the given acronym
anagrams :: DictLetter -> AWord -> AWord -> [AWord]
anagrams dict source acronym =
  map extractAnagram $ search dictRefined sourceRefined acronym
  where
    sourceRefined = filter isAlpha . map toLower $ source
    dictRefined = splitDict acronym dict

search :: Dictionary -> AWord -> AWord -> [Anagram]
search dict source acronym = expand acronym initialState
  where initialState = (MS.empty, wordLetters source, dict)

extractAnagram :: Anagram -> AWord
extractAnagram = unwords . MS.toList

expand :: AWord -> SearchState -> [Anagram]
expand acronym state@(wordsSoFar, remaining, dict)
  -- We have just done the final word
  | 0 == length acronym && MS.null remaining = [wordsSoFar]
  -- Our final word didn't use up all our letters
  -- TODO: we could remove one function call by filtering these from possible words
  -- Don't think that'd help performance much but it could
  | 0 == length acronym = []
  -- Do we at least have one of each letter in the acronym?
  | not canAcronym = []
  -- At the leaves the function can be considerably optimised
  | 1 == length acronym = leaf state
  -- Just for debug tracing
  | 3 <= length acronym = trace (show wordsSoFar ++ (show $ length $ head dict)) allAnagrams
  -- We have work to do my friends
  | otherwise = allAnagrams
  where
    -- The distinction from possible and usable is this:
    -- possibleWords might be used down the line
    -- usableWords can be used in this spot in the acronym
    -- combining the two would lead to the first letter of the acronym
    -- stripping all non-acronymous words
    -- We filter ALL remaining words, even if it's not just this letter,
    -- because IIRC it ended up being faster this way. at present moment i
    -- don't understand how that's possible, but that's what i remember
    possibleWords = map (filter (canSpell remaining)) dict
    -- THIS ONLY WORKS WHEN THE ACRONYM DOESN'T CONTAIN DUPLICATES
    -- For my pet use case that works but TODO: check for that
    usableWords:newDict = possibleWords
    -- We used to remove words from dictionary that we've used
    -- The acronym constraint makes that unnecessary
    allAnagrams = foldl go [] usableWords
    --allAnagrams = foldl go [] (take 20 usableWords) -- Duplicate of above for profiling
    go anagramsSoFar (word, set) =
      anagramsSoFar ++ expand (tail acronym) (MS.insert word wordsSoFar,
        remaining `MS.difference` set, newDict)
    canSpell letters (_, entryLetters) = entryLetters `MS.isSubsetOf` letters
    canAcronym = wordLetters acronym `MS.isSubsetOf` remaining

leaf :: SearchState -> [Anagram]
leaf (wordsSoFar, remaining, dict) = foldl go [] possibleWords
  where
    go acc (word, _) = (MS.insert word wordsSoFar):acc
    possibleWords = filter canSpell $ head dict
    canSpell (_, wordSet) = wordSet == remaining

fitsAcronym :: Char -> DictEntry -> Bool
fitsAcronym letter (word, _) = head word == letter

wordLetters :: AWord -> Letters
wordLetters = MS.fromList

readDict :: IO DictLetter
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
    -- This is words we start with (that we were told is in the acronym)
    -- in wfaoie, Bill Wurtz told us it contains "of", meaning the o IS of
    certain = []
    -- Adding a few certains of early words can make a fast but
    -- realistic profile case
    --certain = ["of", "whore", "into", "intake"]
    -- Use comments here to select a dictionary to use
    --dictionary = "/usr/share/dict/words"
    dictionary = "10000.txt"
    --dictionary = "freq-then-words.txt"
    -- In this dictionary I've removed words we've already computed
    --dictionary = "unknown.txt"
    toDict = map toDictEntry
    toDictEntry word = (word, wordLetters word)

splitDict :: String -> DictLetter -> Dictionary
splitDict acronym partial = ordered
  where
    -- These are dict entries that start with ANY letter in the acronym
    -- Don't remember if this makes groupWith not break or if it's perf
    narrowed = filter (\word -> any (`fitsAcronym` word) acronym) partial
    firstLetter :: DictEntry -> Char
    firstLetter = head . fst
    grouped :: Dictionary
    grouped = groupWith firstLetter narrowed
    -- Why bother maintaining order? There is a BUNCH of optimisation to be
    -- made by processing less common words later
    -- TODO: automate the above
    ordered :: Dictionary
    ordered = foldl orderFunc [] grouped
    orderFunc :: Dictionary -> DictLetter -> Dictionary
    orderFunc acc group = insertBy acronymOrder group acc
    acronymOrder :: DictLetter -> DictLetter -> Ordering
    acronymOrder (((letterA:_), _):_) (((letterB:_), _):_) =
      compare (elemIndex letterA acronym) (elemIndex letterB acronym)
    acronymOrder _ _ = EQ

