--  Author:   Jonathan Jauhari 1038331 <jjauhari@student.unimelb.edu.au>
--  Purpose:  FIXME:
-- 
--  COMP30020 Project 1, S2 2020.

-- FIXME: style guide?

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List
import Control.Applicative

type Chord = [Pitch]
type Score = (Int, Int, Int)

-- type Feedback = (Int, Int, Int)
type GameState = [[Pitch]]

data Pitch = Pitch Char Char deriving Eq

instance Show Pitch where
    show = pitchToString

-- MAYBE Pitch?
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
        commonNotes = (intersectBy' sameNote) target guess
        commonOctaves = (intersectBy' sameOctave) target guess
        noteScore = length (commonNotes \\ commonPitches)
        octaveScore = length (commonOctaves \\ commonPitches)
    in (pitchScore, noteScore, octaveScore)

initialGuess :: ([Pitch], GameState)
initialGuess =
    let pitches = [Pitch note octave | note <- ['A'..'G'], octave <-['1'..'3']]
        initial = [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')]
    in (initial, choose 3 pitches)

-- nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
-- nextGuess _ _ = (
--     [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')],
--     [[(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')]])

nextGuess :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess (guess, state) score =
    let state' = (filter (\t -> feedback t guess == score) state) \\ [guess]
        guess' = head state'
    in (guess', state')

targets :: [[Pitch]]
targets = [
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'A' '3')],
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')],
    [(Pitch 'A' '1'), (Pitch 'B' '1'), (Pitch 'C' '1')],
    [(Pitch 'A' '3'), (Pitch 'B' '2'), (Pitch 'C' '1')],
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')]]

guesses :: [[Pitch]]
guesses = [
    [(Pitch 'A' '1'), (Pitch 'A' '2'), (Pitch 'B' '1')],
    [(Pitch 'A' '1'), (Pitch 'A' '2'), (Pitch 'A' '3')],
    [(Pitch 'A' '2'), (Pitch 'D' '1'), (Pitch 'E' '1')],
    [(Pitch 'C' '3'), (Pitch 'A' '2'), (Pitch 'B' '1')],
    [(Pitch 'A' '1'), (Pitch 'B' '2'), (Pitch 'C' '3')]]

testFeedback :: [(Int, Int, Int)]
testFeedback =
        getZipList $ fmap feedback (ZipList targets) <*> (ZipList guesses)

choose :: (Eq t, Num t) => t -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose k (x:xs) = (map (x:) (choose (k-1) xs)) ++ choose k xs

-- all pitches:
-- [Pitch x y | x <- ['A'..'G'], y <-['1'..'3']]