--  Author:   Jonathan Jauhari 1038331 <jjauhari@student.unimelb.edu.au>
--  Purpose:  Implements both the guessing and answering parts of the
--            logical guessing game "Musician".
-- 
--  COMP30020 Project 2, S2 2020.
--
-- Musician is a game where a composer selects a chord, and a performer
-- repeatedly tries to guess it. A chord is a list of three unique 'pitches',
-- and a pitch comprises a note ('A' to 'G'), and an octave ('1' to '3').
-- The composer gives feedback (the number of correct pitches, correct notes,
-- and correct octaves) to the performer after every incorrect guess, which the
-- performer can use to narrow down guesses.
--
-- To implement both the role of the composer and the performer, this Haskell
-- program defines the following functions:
-- 
-- + toPitch      : parses a String to a Maybe Pitch
-- + feedback     : returns feedback for a target chord and a guessed chord
-- + initialGuess : gives the initial chord to guess and an initial game state
-- + nextGuess    : given the previous guess and state, return the
--                  next guess and state.
--
-- The strategy used to choose the next guess is based on filtering out
-- guesses that are inconsistent with previous guesses, and then choosing the
-- guess that will reduce the number of possibilities by the largest amount.
-- The game state is used to keep track of the remaining possible guesses.

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import Data.Ord

-------------------------------------------------------------------------------

-- | A `Chord` is a list of (three) unique `Pitch`s
type Chord = [Pitch]

-- | A `GameState` is a list of `Chord`'s that are possible targets (answers)
type GameState = [Chord]

-- | A `Feedback` score is a 3-tuple of `Int`s, in order they represent:
-- the number of correct pitches, correct notes, and correct octaves.
type Feedback = (Int, Int, Int)

-- | A `Pitch` comprises a musical note, one of A, B, C, D, E, F, or G,
-- and an octave, one of 1, 2, or 3. Notes and octaves are stored as `Char`s.
data Pitch = Pitch Char Char deriving Eq

-- | A `Pitch` is shown as a string of two characters: its note and octave
instance Show Pitch where
    show (Pitch note octave) = [note, octave]

-- | `note` returns a `Pitch`s note
note :: Pitch -> Char
note (Pitch note _) = note

-- | `octave` returns a `Pitch`s octave
octave :: Pitch -> Char
octave (Pitch _ octave) = octave

-- | `toPitch` gives `Just` the `Pitch` named by the given string,
-- or `Nothing` if the string is not a valid pitch name.
toPitch :: String -> Maybe Pitch
toPitch [note, octave] =
    if note `elem` ['A'..'G'] && octave `elem` ['1'..'3'] then
        Just (Pitch note octave)
    else
        Nothing
toPitch _ = Nothing

-------------------------------------------------------------------------------

-- | `sameNote` checks whether the two given `Pitch`'s have the same note
sameNote :: Pitch -> Pitch -> Bool
sameNote p1 p2 = note p1 == note p2

-- | `sameOctave` checks whether the two given `Pitch`'s have the same octave
sameOctave:: Pitch -> Pitch -> Bool
sameOctave p1 p2 = octave p1 == octave p2

-- | `intersectBy\'` returns the intersection between two arrays As and Bs:
-- a list that contains all the elements in As for which there is a
-- corresponding matching element in Bs, allowing duplicates.
-- e.g. 2 elements in As matching 2 elements in Bs results in a
--      list containing the 2 elements in A.
--
-- Whether an element in As matches an element in Bs is decided by the
-- input predicate. If two elements in As match an element in Bs, the one
-- that occurs later in As is preferred.
intersectBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' predicate as bs =
    let minus = deleteFirstsBy predicate
    in  as `minus` (as `minus` bs)

-- | `feedback` returns the `Feedback` score, a triple of the number of
-- correct pitches, notes and octaves, for a given guess and target
-- (both assumed to contain only unique `Pitch`s).
--
-- In counting correct notes and octaves, multiple occurrences in the guess
-- are only counted as correct if they also appear repeatedly in the target.
-- Correct pitches are not also counted as correct notes and octaves.
feedback :: Chord -> Chord -> Feedback
feedback target guess =
    let commonPitches = target `intersect` guess
        pitchScore = length commonPitches
        target' = target \\ commonPitches
        guess' = guess \\ commonPitches
        noteScore = length $ (intersectBy' sameNote) target' guess'
        octaveScore = length $ (intersectBy' sameOctave) target' guess'
    in (pitchScore, noteScore, octaveScore)

-------------------------------------------------------------------------------

-- | `initialGuess` returns the first `Chord` to guess and the initial
-- `GameState`. The initial guess is chosen to eliminate the as many possible
-- targets early on.
--
-- The `Chord` [A2, B1, C1] was one of the initial `Chord`s that resulted in a
-- very low (around 4.21 in tests) number of guesses to guess the correct
-- `Chord`, on average, after "brute-force" checking over all 1330 possible
-- target `Chord`s.
initialGuess :: (Chord, GameState)
initialGuess =
    let allPitches = [ Pitch note octave
                     | note <- ['A'..'G']
                     , octave <-['1'..'3']]
        initial = [(Pitch 'A' '2'), (Pitch 'B' '1'), (Pitch 'C' '1')]
        state = combinations 3 allPitches
    in (initial, state)

-- | `nextGuess` returns the next `Chord` to be guessed and the corresponding
-- `GameState`, given the previous guess and game state.
--
-- Inconsistent guesses are filtered out (this includes the previous guess,
-- since it must have been incorrect), and from the remaining possibilities,
-- the one that results in the fewest expected remaining targets is picked.
nextGuess :: (Chord, GameState) -> Feedback -> (Chord, GameState)
nextGuess (guess, state) score = 
    let state' = (filter (\target -> feedback target guess == score) state)
        guess' = pickBest state'
    in (guess', state')

-- | `pickBest` returns the `Chord` that results in the fewest expected
-- remaining possibilities if guessed, given the remaining possible targets
-- in `GameState`.
pickBest :: GameState -> Chord
pickBest state =
    minimumBy (comparing (expectedRemainingCandidates state)) state

-- | `expectedRemainingCandidates`  returns the expected number of remaining
-- possible condidates if the input `Chord` guess were chosen from among the
-- other remaining possibilities in the given game state.
--
-- This is computed by grouping guesses that would result in the same
-- feedback score, assuming the given guess is the actual target, then
-- summing up the products of length and relative frequency (probability)
-- of each group. This sum corresponds to the expected length of the list
-- containing all the remaining candidates resulting from the guess. The
-- smaller this is, the better the guess is at eliminating possibilities.
expectedRemainingCandidates ::  GameState -> Chord -> Double
expectedRemainingCandidates state guess =
    let nPossibilities = length state
        possibleFeedbacks = fmap ((flip feedback) guess) state
        lengths = fmap genericLength ((group . sort) possibleFeedbacks)
        component = \len -> len * (len / fromIntegral nPossibilities)
    in  sum (fmap component lengths)

-- | `combinations` returns a list of lists: each list is combination of
-- elements in the input list; each combination is of the given length k.
combinations :: Int -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) = (map (x:) (combinations (k-1) xs)) ++ combinations k xs

-------------------------------------------------------------------------------
