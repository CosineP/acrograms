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
import Debug.Trace (trace)

type AWord = Text
type Dictionary = Set AWord

type Anagram = MultiSet AWord
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- | Generate anagrams of the given word using the dictionary supplied.
-- The anagrams with the fewest words are returned first, which can lead to
-- high memory usage.
anagrams :: Dictionary -> Text -> Text -> [Text]
anagrams dict source acronym =
  map extractAnagram $ catMaybes $ breadthFirstNodes $ search dict source acronym
  where breadthFirstNodes = concat . Tr.levels

search :: Dictionary -> Text -> Text -> Tree (Maybe Anagram)
search dict source acronym = Tr.unfoldTree (expand acronym) initialState
  where initialState = (MS.empty, wordLetters source, dict)

extractAnagram :: Anagram -> Text
extractAnagram = T.unwords . MS.toList

expand :: Text -> SearchState -> (Maybe Anagram, [SearchState])
expand acronym (wordsSoFar, remaining, dict)
  -- We cannot possibly construct an acronym long enough with this many letters
  | length wordsSoFar + MS.size remaining < T.length acronym = (Nothing, [])
  | otherwise = (completeAnagram, nextStates)
  where
    completeAnagram = if MS.null remaining then Just wordsSoFar else Nothing
    -- The distinction from possible and usable is this:
    -- possibleWords might be used down the line
    -- usableWords can be used in this spot in the acronym
    -- combining the two would lead to the first letter of the acronym
    -- stripping all non-acronymous words
    possibleWords = S.filter (canSpell remaining) dict
    usableWords = S.filter (fitsAcronym) possibleWords
    -- As we generate new branches, we remove words for which we have
    -- already created a branch: this ensures that independent branches
    -- will not generate identical sets of words.
    nextStates = fst $ foldl go ([], possibleWords) $ usableWords
    go (states, d) word =
      ((MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d):states,
       S.delete word d)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters
    fitsAcronym word = (head $ T.unpack word) == nextLetter
    -- TODO: add guard for too many words rather than panic
    nextLetter = T.unpack acronym !! length wordsSoFar

wordLetters :: Text -> Letters
wordLetters = MS.fromList . filter isAlpha . T.unpack . T.toLower


readDict :: IO Dictionary
readDict = (S.filter goodWord . S.fromList . T.lines) <$> TIO.readFile "/usr/share/dict/words"
  where goodWord "A" = True
        goodWord "I" = True
        goodWord "O" = True
        goodWord w   = T.length w > 1
