--  Author:   Jonathan Jauhari 1038331 <jjauhari@student.unimelb.edu.au>
--  Purpose:  Evaluating cribbage hands and deciding which card(s) to keep in
--            the hand to maximise score.
-- 
--  COMP30020 Project 1, S2 2020.

module Proj2 (Pitch, toPitch, feedback,
              GameState, initialGuess, nextGuess) where

import Data.List

-- type Feedback = (Int, Int, Int)
type GameState = ()

data Pitch = Pitch Char Char

instance Show Pitch where
    show = pitchToString

-- MAYBE Pitch?
pitchToString :: Pitch -> String
pitchToString (Pitch note octave) = [note, octave]


toPitch :: String -> Maybe Pitch
toPitch [note, octave] =
    if note `elem` ['A'..'G'] && octave `elem` ['1'..'3'] then
        Just (Pitch note octave)
    else
        Nothing
toPitch _ = Nothing


intersectBy' :: (a -> a -> Bool) -> [a] -> [a] -> [a]
intersectBy' eq as bs =
    let minus = deleteFirstsBy eq in
    as `minus` (as `minus` bs)

feedback :: [Pitch] -> [Pitch] -> (Int, Int, Int)
feedback p1s p2s = (0,0,0)

-- TODO:
feedback' :: (Num a1, Num b, Num c) => [a2] -> [a3] -> (a1, b, c) -> (a1, b, c)
feedback' [] [] state = state
feedback' (p1:p1s) (p2:p2s) (cPitches, cNotes, cOctaves) = (0,0,0)

-- TODO:
initialGuess :: a -> a
initialGuess = id

-- TODO:
nextGuess :: a -> a
nextGuess = id