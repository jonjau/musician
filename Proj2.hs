--  Author:   Jonathan Jauhari 1038331 <jjauhari@student.unimelb.edu.au>
--  Purpose:  Implements the guessing part of the logical guessing game
--            "Musician".
-- 
--  COMP30020 Project 1, S2 2020.
-- FIXME: style guide?


-- module Proj2 (Pitch, toPitch, feedback,
--               GameState, initialGuess, nextGuess) where

-- FIXME: I added some exports
module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess, allPitches, nextGuess',
              testFeedback) where

import Data.List
import Control.Applicative
import Data.Ord

-- type Feedback = (Int, Int, Int)
type GameState = [[Pitch]]
-- type GameState = (([Pitch], (Int, Int, Int)), [[Pitch]])

data Pitch = Pitch Char Char deriving Eq

-- FIXME: delete maybe?
type Chord = [Pitch]
type Score = (Int, Int, Int)

instance Show Pitch where
    show = pitchToString

pitchToString :: Pitch -> String
pitchToString (Pitch note octave) = [note, octave]

note :: Pitch -> Char
note (Pitch note _) = note

octave :: Pitch -> Char
octave (Pitch _ octave) = octave

toPitch :: String -> Maybe Pitch
toPitch [note, octave] =
    if note `elem` ['A'..'G'] && octave `elem` ['1'..'3'] then
        Just (Pitch note octave)
    else
        Nothing
toPitch _ = Nothing


intersectBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' eq as bs =
    let minus = deleteFirstsBy eq
    in  as `minus` (as `minus` bs)

feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback target guess =
    let commonPitches = target `intersect` guess -- works since pitches unique
        pitchScore = length commonPitches
        sameNote = \t g -> note t == note g
        sameOctave = \t g -> octave t == octave g
        target' = target \\ commonPitches
        guess' = guess \\ commonPitches
        commonNotes = (intersectBy' sameNote) target' guess'
        commonOctaves = (intersectBy' sameOctave) target' guess'
        noteScore = length commonNotes
        octaveScore = length commonOctaves
    in (pitchScore, noteScore, octaveScore)

-- exclude this guess in initialGuess or nextGuess?
-- FIXME: document how initial guess was obtained
initialGuess :: ([Pitch], GameState)
initialGuess =
    let pitches = [Pitch note octave | note <- ['A'..'G'], octave <-['1'..'3']]
        initial = [(Pitch 'A' '2'), (Pitch 'B' '1'), (Pitch 'C' '1')]
    in (initial, choose 3 pitches)

-- nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
-- nextGuess _ _ = (
--     [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')],
--     [[(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')]])

-- | make guesses that are consistent
-- 4.8240601503759395
nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (guess, state) score =
    let state' = (filter (\t -> feedback t guess == score) state) \\ [guess]
        guess' = head state'
    in (guess', state')

nextGuess' :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess' (guess, state) score = 
    let state' = (filter (\t -> feedback t guess == score) state) \\ [guess]
        guess' = pick state'
    in (guess', state')

-- expectedRemainingTargets :: GameState -> [Pitch] -> [Int]
expectedRemainingTargets :: Fractional a => [[Pitch]] -> [Pitch] -> a
expectedRemainingTargets state guess =
    let 
        state' = delete guess state
        nPossibilities = length state'
        possibleFeedbacks = fmap (flip feedback guess) state'
        lengths = fmap length (group (sort possibleFeedbacks))
        comp = \length ->
            (fromIntegral length) * (
                (fromIntegral length) / (fromIntegral nPossibilities))
    in  sum (fmap comp lengths)

pick :: GameState -> [Pitch]
pick state = minimumBy (comparing (expectedRemainingTargets state)) state


targets :: [[Pitch]]
targets = [
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'A' '3')],
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')],
    [(Pitch 'A' '1'), (Pitch 'B' '1'), (Pitch 'C' '1')],
    [(Pitch 'A' '3'), (Pitch 'B' '2'), (Pitch 'C' '1')],
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')],
    [(Pitch 'A' '1'), (Pitch 'F' '1'), (Pitch 'F' '2')]]

guesses :: [[Pitch]]
guesses = [
    [(Pitch 'A' '1'), (Pitch 'A' '2'), (Pitch 'B' '1')],
    [(Pitch 'A' '1'), (Pitch 'A' '2'), (Pitch 'A' '3')],
    [(Pitch 'A' '2'), (Pitch 'D' '1'), (Pitch 'E' '1')],
    [(Pitch 'C' '3'), (Pitch 'A' '2'), (Pitch 'B' '1')],
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')],
    [(Pitch 'G' '3'), (Pitch 'F' '1'), (Pitch 'D' '1')]]

-- [(1,2,1),(1,0,2),(0,1,2),(0,3,3),(3,0,0),(1,0,1)]
testFeedback :: [(Int, Int, Int)]
testFeedback =
        getZipList $ fmap feedback (ZipList targets) <*> (ZipList guesses)

-- | Combinations
choose :: (Eq t, Num t) => t -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose k (x:xs) = (map (x:) (choose (k-1) xs)) ++ choose k xs

-- all pitches:
-- [Pitch x y | x <- ['A'..'G'], y <-['1'..'3']]

-- feedback [A1,F1,F2] [G3,F1,D1] = (1,1,1) -- should be (1,0,1)

--------------------------------------------------------------------------------

allPitches :: [[Pitch]]
allPitches = choose 3 [Pitch x y | x <- ['A'..'G'], y <-['1'..'3']]