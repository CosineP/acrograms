{-# LANGUAGE OverloadedStrings #-}
module Anagrams(anagrams, readDict) where

import           Control.Applicative ((<$>))
import           Data.Char           (isAlpha)
import           Data.Maybe          (catMaybes)
import           Data.MultiSet       (MultiSet)
import qualified Data.MultiSet       as MS
import           Data.Set            (Set)
import qualified Data.Set            as S
import           Data.Text           (Text)
import qualified Data.Text           as T
import qualified Data.Text.IO        as TIO
import           Data.Tree           (Tree)
import qualified Data.Tree           as Tr

type AWord = Text
type Dictionary = Set AWord

type Anagram = MultiSet AWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- | Generate anagrams of the given word using the dictionary supplied.
-- | They also meet the requirements of the given acronym
anagrams :: Dictionary -> Text -> Text -> [Text]
anagrams dict source acronym =
  map extractAnagram $ search narrowedDict source acronym
  where
    narrowedDict = S.filter (\word -> any (`fitsAcronym` word) (T.unpack acronym)) dict

search :: Dictionary -> Text -> Text -> [Anagram]
search dict source acronym = expand acronym initialState
  where initialState = (MS.empty, wordLetters source, dict)

extractAnagram :: Anagram -> Text
extractAnagram = T.unwords . MS.toList

expand :: Text -> SearchState -> [Anagram]
expand acronym (wordsSoFar, remaining, dict)
  -- We have just done the final word
  | length wordsSoFar == T.length acronym && MS.null remaining = [wordsSoFar]
  -- Our final word didn't use up all our letters
  -- TODO: we could remove one function call by filtering these from possible words
  -- Don't think that'd help performance much but it could
  | length wordsSoFar == T.length acronym = []
  -- We have work to do my friends
  | otherwise = anagrams
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
    anagrams = fst $ foldl go ([], newDict) $ usableWords
    go (anagrams, d) word =
      (anagrams ++ expand acronym (MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d),
       S.delete word d)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters
    nextLetter = T.unpack acronym !! length wordsSoFar

fitsAcronym :: Char -> Text -> Bool
fitsAcronym letter word = (head $ T.unpack word) == letter

wordLetters :: Text -> Letters
wordLetters = MS.fromList . T.unpack


readDict :: IO Dictionary
readDict = (S.filter goodWord . (S.map T.toLower) . S.fromList . T.lines) <$> TIO.readFile dictionary
  where goodWord "a" = True
        goodWord "i" = True
        --goodWord "O" = True -- bill wouldn't use this one
        goodWord w   = T.length w > 1
        --dictionary = "/usr/share/dict/words"
        dictionary = "10000.txt"
