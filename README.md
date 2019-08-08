# acrograms

> find anagrams that also form a specific acronym

**The code is forked from this useful working example of anagrams in haskell:
[purcell/haskell-anagrams](https://github.com/purcell/haskell-anagrams)**

imagine you have the phrase "drunk video: if i'm in Greenland, i follow UFOs
to Antarctica", and you want an anagram that fits the acronym "wfaoie". you
can use this program, and you will find the solution: "Wild Frolicking
Adventures of Informational Education"

why in the hell would you want to do that? well because that's actually a
real example, from bill wurtz, and that is from where this program originates

## usage

    cabal run "wfaoie" "drunk video: if i'm in Greenland, i follow UFOs to Antarctica" +RTS -N

also take a look at `readDict`, where there are some compile-time options
to consider such as:

- `certain`: words that are certainly in the solution
- `dictionary`: file to read valid words from (all lowercase, one word per
  line)

## wfaoie

february 2019. [bill wurtz](https://billwurtz.com/) has been working
on a new non-music video for almost a month. we're all wondering what it
is. what it's about. february 5 we're blessed with a piece of information: the
[initials](https://www.instagram.com/p/Btfak8eB_6x/) of the title, `wfaoie`. on
it's own it's mostly useless, if exciting. but on the 10th, we were given
an [anagram](https://billwurtz.com/questions/q.php?date=201902101445) as
well. an anagram on its own gives much potential, but has a massive solution
space and it's anyone's say if an answer is "right". along with the acronym
though, the solution space is much smaller. even, computable. so that's what
[i set out to do](https://anticapitalist.party/@cosine/101572853339503140)

## technical and fork details

the main differences from upstream in the standard anagram finding are for
optimization:

- DFS
  - since the solution space is so small, the order of searching is not as
  important as with plain anagrams
  - BFS was taking >16GB of memory for the size of anagram i needed
- String is actually faster than Data.Text AND Bytes, believe it or not

then of course there's the acronym-specific code. the general strategy is:

- recursively:
- filter words that fit the remaining letters of the anagram
- do not filter words based on acronym, but do ensure that tested words fit
  the acronym
  - believe it or not it's faster that way
- if not, try another set of words

there are a lot of subtle techniques used for optimization, which are outlined
in my [fedi thread](https://anticapitalist.party/@cosine/101572853339503140)

