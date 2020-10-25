-- %2020-10-18
-- Tests and experiments for Project 2

import Proj2
    ( feedback, initialGuess, nextGuess, toPitch, GameState, Pitch )
import Data.List
    ( minimumBy, (\\), delete, genericLength, intersect, union )
import Control.Applicative ( ZipList(ZipList, getZipList) )
import Data.Maybe ( fromJust )
import Data.Ord ( comparing )
import Data.List

help :: [Char] -> (Char, Char)
help [a,b] = (a,b)
help _ = ('x','y')

data Suit = Club | Diamond | Heart | Spade
    deriving (Show, Eq, Ord)
data Rank
    = R2 | R3 | R4 | R5 | R6 | R7 | R8 | R9 | R10
    | Jack | Queen | King | Ace
    deriving Show

data Card = Card Suit Rank deriving Show

type Person = String

fun :: Person -> String
fun p = p ++ " the person"

-- https://blog.ramdoot.in/monadic-do-block-yet-again-a98cf0237b25
-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers

-- maybeP :: Maybe Pitch -> Pitch
-- maybeP (Just p) = p
-- maybeP Nothing = pitch

-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,10,20]
-- evaluates to [101, 12, 23]
-- equivalently:
-- getZipList $ fmap (+) (ZipList [1,2,3]) <*> (ZipList [100,10,20])
-- how does one make this work with ZipList [Just 1, Just 2, Just 3] ?

-- (fmap (*2)) <$> [Just 1, Nothing, Just 3]
-- evaluates to [Just 2, Nothing, Just 6]

getPitch :: IO (Maybe Pitch)
getPitch = do
    line <- getLine
    return $ toPitch line

testGroup :: Eq a => [a] -> [a] -> ([a], [a], [a])
testGroup as bs = (as \\ int, bs \\ int, int) where int = intersect as bs

correctPitches :: Eq a => [a] -> [a] -> Int
correctPitches target guess = length (target `intersect` guess)

intersect' :: Eq a => [a] -> [a] -> [a]
intersect' a b = a \\ (a \\ b)

sameInt :: Int -> Int -> Bool
sameInt = (==)

mkPitches :: [String] -> [Maybe Pitch]
mkPitches xs = fmap toPitch xs

powerset :: Eq a => [a] -> [[a]]
powerset [] = [[]]
powerset s@(x:_) =
    let s' = s \\ [x]
        p  = powerset(s')
    in  p `union` [s `union` [x] | s <- p]

-- main :: IO ()
-- main = do
--     -- putStrLn "Hello World. Pitch?"
--     -- a <- getPitch
--     -- case a of
--     --     Just _ -> putStrLn "good pitch"
--     --     Nothing -> putStrLn "bad pitch"
--     let res = 3
--     print $ res

-- a do-block that is not for IO !
testdo :: [Maybe Integer]
testdo = do
    f <- [(*2), (+2)]
    m <- [Just 1, Just 2, Nothing]
    pure (f <$> m)

onceAgain :: (Applicative f, Num b) => [f b] -> [f b] -> [f b]
onceAgain xs ys = [pure (+) <*> x <*> y | (x,y) <- zip xs ys]

--------------------------------------------------------------------------------

choose :: (Eq t, Num t) => t -> [a] -> [[a]]
choose 0 _  = [[]]
choose _ [] = []
choose k (x:xs) = (map (x:) (choose (k-1) xs)) ++ choose k xs

guessTest' :: ([Pitch], GameState) -> [Pitch] -> Int
guessTest' (guess, state) target =
    mguess guess state target 1

mguess :: [Pitch] -> GameState -> [Pitch] -> Int -> Int
mguess guess state target nGuesses =
    if target == guess then nGuesses
    else
        let score = feedback target guess
            (guess', state')= nextGuess (guess, state) score
        in mguess guess' state' target (nGuesses + 1)

-- | Parse a string containing a number of space-separated pitches to produce
-- a list of pitches.  Error if any of the pitches can't be parsed.
toChord :: String -> [Pitch]
toChord = (fromJust . mapM toPitch . words)

averageNumberOfGuesses :: ([Pitch], GameState) -> [[Pitch]] -> Double
averageNumberOfGuesses initialGuess targets =
    let 
        allNumberOfGuesses = fmap (guessTest' initialGuess) targets
        avgNumberOfGuesses =
            realToFrac
                (sum allNumberOfGuesses) /(genericLength allNumberOfGuesses)
    in avgNumberOfGuesses

mysplit :: [[Pitch]] -> [([Pitch], [[Pitch]])]
mysplit [] = []
mysplit l@(p:ps) = (p, delete p l) : mysplit ps


main :: IO ()
main = do
  -- putStr "Target chord (3 pitches separated by spaces): "
  let allTargets = allChords
  let allInitialGuesses = fmap (\t -> (t, allChords \\ [t])) allChords
  let initials = [initialGuess]
  let ys = fmap (flip averageNumberOfGuesses allTargets) initials
  let anss = zip initials ys
  let best = minimumBy (comparing snd) anss
  let ((init, _), nguesses) = best
  putStrLn $ show init
  putStrLn $ show nguesses

  -- let ans = averageNumberOfGuesses initialGuess allTargets

 --   let allNGuesses = fmap (guessTest' initialGuess) allTargets
--   let avgNGuesses = realToFrac (sum allNGuesses) / (genericLength allNGuesses)

-- | just make guesses that are consistent, this one isn't very clever
--  average: 4.8240601503759395
nextGuess' :: ([Pitch], GameState) -> (Int, Int, Int) -> ([Pitch], GameState)
nextGuess' (guess, state) score =
    let state' = (filter (\t -> feedback t guess == score) state) \\ [guess]
        guess' = head state'
    in (guess', state')

targets :: [[Pitch]]
targets = [
    toChord "A1 B2 A3",
    toChord "A1 B2 C3",
    toChord "A1 B1 C1",
    toChord "A3 B2 C1",
    toChord "A1 B2 C3",
    toChord "A1 F1 F2"]

guesses :: [[Pitch]]
guesses = [
    toChord "A1 A2 B1",
    toChord "A1 A2 A3",
    toChord "A2 D1 E1",
    toChord "C3 A2 B1",
    toChord "A1 B2 C3",
    toChord "G3 F1 D1"]

-- | expected: [(1,2,1),(1,0,2),(0,1,2),(0,3,3),(3,0,0),(1,0,1)]
testFeedback :: [(Int, Int, Int)]
testFeedback =
        getZipList $ fmap feedback (ZipList targets) <*> (ZipList guesses)

-- | `combinations` returns a list of lists: each list is combination of
--   elements in the input list; each combination is of the given length k.
combinations :: (Eq t, Num t) => t -> [a] -> [[a]]
combinations 0 _  = [[]]
combinations _ [] = []
combinations k (x:xs) =(map (x:) (combinations (k-1) xs)) ++ combinations k xs

allChords :: [[Pitch]]
allChords = combinations 3 [ (fromJust . toPitch) [x,y]
                           | x <- ['A'..'G']
                           , y <-['1'..'3']]

type Chorda = [Pitch]

expectedRemainingCandidates' ::  GameState -> Chorda -> Double
expectedRemainingCandidates' state guess =
    let 
        nPossibilities = length state
        possibleFeedbacks = fmap ((flip feedback) guess) state
        lengths = fmap genericLength ((group . sort) possibleFeedbacks)
        comp = \len -> len * (len / fromIntegral nPossibilities)
    in  sum (fmap comp lengths)

-- -- expect (1/10) * 1 + (3/10) * 3 + (6/10) * 6 = 4.6
testERT :: Double
testERT =
    let state = testChords
        guess = toChord "A1 B2 C3"
    in expectedRemainingCandidates' state guess

testChords :: GameState
testChords = [ toChord "A1 B2 C3" -- (3,0,0) correct

             , toChord "A1 D2 E3" -- (1,0,2)
             , toChord "A1 E2 F3"
             , toChord "A1 F2 G3"

             , toChord "A1 B2 A3" -- (2,0,1)
             , toChord "A1 B2 B3"
             , toChord "A1 B2 D3"
             , toChord "A1 B2 E3"
             , toChord "A1 B2 F3"
             , toChord "A1 B2 G3"
             ]

-- With A1 B2 C3:
--
-- Filtering inconsistent guesses:
-- 4.8240601503759395
--
-- real    0m0.684s
-- user    0m0.654s
-- sys     0m0.000s

-- Filtering inconsistent guesses + O2 compiler flag:
-- 4.8240601503759395
--
-- real    0m0.381s
-- user    0m0.339s
-- sys     0m0.010s


-- Filtering impossible guesses + picking best guess:
-- 4.272180451127819
--
-- real    2m19.625s
-- user    2m19.211s
-- sys     0m0.380s

-- Filtering impossible guesses + picking best guess + O2 compiler flag:
-- 4.272180451127819
--
-- real    1m17.186s
-- user    1m16.947s
-- sys     0m0.210s

-- Filtering impossible guesses + picking best guess + O2 compiler flag:
-- 530 targets
-- 4.3830188679245285
-- real    0m36.746s
-- user    0m36.591s
-- sys     0m0.120s

-- A2 B1 C1
-- Filtering impossible guesses + picking best guess + O2 compiler flag:
-- 4.207518796992481
-- real    0m44.984s
-- user    0m44.797s
-- sys     0m0.160s

-- B1 D1 F2
-- Filtering impossible guesses + picking best guess + O2 compiler flag:
-- 4.2105263157894735
--
-- real    0m44.449s
-- user    0m44.282s
-- sys     0m0.140s

-- B1 D2 F2
-- Filtering impossible guesses + picking best guess + O2 compiler flag:
-- 4.211278195488722
--
-- real    0m45.217s
-- user    0m45.106s
-- sys     0m0.080s

-- A1,B1,C2
-- Filtering impossible guesses + picking best guess + O2 compiler flag:
-- 4.207518796992481
--
-- real    0m55.110s
-- user    0m54.839s
-- sys     0m0.240s

-- [A2,B1,C1]
-- 4.207518796992481
--
-- real    0m55.742s
-- user    0m55.459s
-- sys     0m0.250s

-- [A2,B1,C1]
-- 4.208270676691729

-- real    0m49.519s
-- user    0m49.350s
-- sys     0m0.140s
