
import Proj2

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


-- https://blog.ramdoot.in/monadic-do-block-yet-again-a98cf0237b25
-- https://en.wikibooks.org/wiki/Haskell/Monad_transformers

-- maybeP :: Maybe Pitch -> Pitch
-- maybeP (Just p) = p
-- maybeP Nothing = pitch


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

main :: IO ()
main = do
    -- putStrLn "Hello World. Pitch?"
    -- a <- getPitch
    -- case a of
    --     Just _ -> putStrLn "good pitch"
    --     Nothing -> putStrLn "bad pitch"
    let res = correctPitches [1,2,3] [5,0,4]
    print $ res


