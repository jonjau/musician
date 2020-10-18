
import Proj2

import Data.List
import Control.Monad
import Control.Applicative
import Data.Maybe
import Data.Ord

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

-- https://blog.ramdoot.in/monadic-do-block-yet-again-a98cf0237b25
-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers

-- maybeP :: Maybe Pitch -> Pitch
-- maybeP (Just p) = p
-- maybeP Nothing = pitch

-- getZipList $ (+) <$> ZipList [1,2,3] <*> ZipList [100,10,20]
-- evaluates to [101, 12, 23]
-- equivalently:
-- getZipList $ fmap (+) (ZipList [1,2,3]) <*> (ZipList [100,10,20])
-- FIXME: how does one make this work with ZipList [Just 1, Just 2, Just 3] ?


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

-- | Guess the given target, counting and showing the guesses.
guessTest :: [Pitch] -> IO ()
guessTest target = do
    let (guess, other) = initialGuess
    loop target guess other 1

-- guessTest' :: [[Pitch]] -> [Pitch] -> GameState -> Int -> Int
-- guessTest' [] guess state nGuesses = nGuesses
-- guessTest' (t:ts) guess state nGuesses =
--     if 

guessTest' :: ([Pitch], GameState) -> [Pitch] -> Int
guessTest' (guess, state) target =
    mguess' guess state target 1

-- Filtering impossible guesses + picking best guess:

mguess :: [Pitch] -> GameState -> [Pitch] -> Int -> Int
mguess guess state target nGuesses =
    if target == guess then nGuesses
    else
        let score = feedback target guess
            (guess', state')= nextGuess (guess, state) score
        in mguess guess' state' target (nGuesses + 1)

mguess' :: [Pitch] -> GameState -> [Pitch] -> Int -> Int
mguess' guess state target nGuesses =
    if target == guess then nGuesses
    else
        let score = feedback target guess
            (guess', state')= nextGuess' (guess, state) score
        in mguess' guess' state' target (nGuesses + 1)

-- | Given a target and guess and a guess number, continue guessing
-- until the right target is guessed.
loop :: [Pitch] -> [Pitch] -> GameState -> Int -> IO ()
loop target guess other guesses = do
  putStrLn $ "Your guess #" ++ show guesses ++ ":  " ++ show guess
  let answer = feedback target guess
  putStrLn $ "    My answer:  " ++ show answer
  if answer == (3, 0, 0)
    then do
      putStrLn $ "You got it in " ++ show guesses ++ " guesses!"
    else do
      let (guess', other') = nextGuess (guess, other) answer
      loop target guess' other' (guesses + 1)

-- | Parse a string containing a number of space-separated pitches to produce
-- a list of pitches.  Error if any of the pitches can't be parsed.
toChord :: String -> [Pitch]
toChord = (fromJust . mapM toPitch . words)

averageNumberOfGuesses :: ([Pitch], GameState) -> [[Pitch]] -> Double
averageNumberOfGuesses initialGuess targets =
    let 
        allNumberOfGuesses = fmap (guessTest' initialGuess) targets
        avgNumberOfGuesses =
            realToFrac (sum allNumberOfGuesses) / (genericLength allNumberOfGuesses)
    in avgNumberOfGuesses

mysplit :: [[Pitch]] -> [([Pitch], [[Pitch]])]
mysplit [] = []
mysplit l@(p:ps) = (p, delete p l) : mysplit ps


-- | Prompt for a target and use guessTest to try to guess it.
main :: IO ()
main = do
  -- putStr "Target chord (3 pitches separated by spaces): "
  let allTargets = allPitches
  let allInitialGuesses = fmap (\t -> (t, allPitches \\ [t])) allPitches
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