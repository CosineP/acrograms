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

type Word = Text
type Dictionary = Set Word

type Anagram = MultiSet Word
type Letters = MultiSet Char

type SearchState = (Anagram, Letters, Dictionary)

-- | Generate anagrams of the given word using the dictionary supplied.
-- The anagrams with the fewest words are returned first, which can lead to
-- high memory usage.
anagrams :: Dictionary -> Text -> [Text]
anagrams dict source =
  map extractAnagram $ catMaybes $ breadthFirstNodes $ search dict source
  where breadthFirstNodes = concat . Tr.levels

search :: Dictionary -> Text -> Tree (Maybe Anagram)
search dict source = Tr.unfoldTree expand initialState
  where initialState = (MS.empty, wordLetters source, dict)

extractAnagram :: Anagram -> Text
extractAnagram = T.unwords . MS.toList

expand :: SearchState -> (Maybe Anagram, [SearchState])
expand (wordsSoFar, remaining, dict) = (completeAnagram, nextStates)
  where
    completeAnagram = if MS.null remaining then Just wordsSoFar else Nothing
    possibleWords = S.filter (remaining `canSpell`) dict
    -- As we generate new branches, we remove words for which we have
    -- already created a branch: this ensures that independent branches
    -- will not generate identical sets of words.
    nextStates = fst $ foldl go ([], possibleWords) $ S.toList possibleWords
    go (states, d) word =
      ((MS.insert word wordsSoFar,
        remaining `MS.difference` wordLetters word, d):states,
       S.delete word d)
    canSpell letters word = wordLetters word `MS.isSubsetOf` letters

wordLetters :: Text -> Letters
wordLetters = MS.fromList . filter isAlpha . T.unpack . T.toLower


readDict :: IO Dictionary
readDict = (S.filter goodWord . S.fromList . T.lines) <$> TIO.readFile "/usr/share/dict/words"
  where goodWord "A" = True
        goodWord "I" = True
        goodWord "O" = True
        goodWord w   = T.length w > 1
