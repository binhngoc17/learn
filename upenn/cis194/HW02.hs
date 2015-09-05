{-# OPTIONS_GHC -Wall #-}
module HW02 where
import qualified Data.List

-- Mastermind -----------------------------------------

-- A peg can be one of six colors
data Peg = Red | Green | Blue | Yellow | Orange | Purple
         deriving (Show, Eq, Ord)

-- A code is defined to simply be a list of Pegs
type Code = [Peg]

-- A move is constructed using a Code and two integers; the number of
-- exact matches and the number of regular matches
data Move = Move Code Int Int
          deriving (Show, Eq)

-- List containing all of the different Pegs
colors :: [Peg]
colors = [Red, Green, Blue, Yellow, Orange, Purple]

-- Exercise 1 -----------------------------------------
-- Helpers

boolToInt :: Bool -> Int
boolToInt b
    | b == True = 1
    | otherwise = 0

-- Get the number of exact matches between the actual code and the guess
exactMatches :: Code -> Code -> Int
exactMatches [] [] = 0
exactMatches [] xs = 0
exactMatches ys [] = 0
exactMatches (x:ys) (a:bs) = (boolToInt (x == a)) + (exactMatches ys bs)

--Minh's solution
--exactMatches a b = length . filter (uncurry (==)) $ zip a b

-- Exercise 2 -----------------------------------------

-- For each peg in xs, count how many times is occurs in ys
countColors :: Code -> [Int]
countColors codes = map (\x -> length $ filter (== x) codes) colors

-- Count number of matches between the actual code and the guess
matches :: Code -> Code -> Int
--matches code1 code2 = (sum $ (uncurry min) $ zip (countColors code1) (countColors code2))
matches code1 code2 = (sum $ map (uncurry (\x y -> min x y)) $ zip (countColors code1) (countColors code2))

--Minh's solution
--matches xs ys = sum . map (uncurry min) $ (zip `on` countColors) xs ys

-- Exercise 3 -----------------------------------------

-- Construct a Move from a guess given the actual code
getMove :: Code -> Code -> Move
getMove actual guess = Move guess exactMatchCount nonExactMatchCount
    where   exactMatchCount    = (exactMatches actual guess)
            nonExactMatchCount = ((matches actual guess) - exactMatchCount)

-- Exercise 4 -----------------------------------------

isConsistent :: Move -> Code -> Bool
isConsistent (Move guess exactMatchCount nonExactMatchCount) code = exactMatchEqual && nonExactMatchEqual
    where exactMatchEqual = ((exactMatches code guess) == exactMatchCount)
          nonExactMatchEqual = (((matches guess code) - exactMatchCount) == nonExactMatchCount)

-- Exercise 5 -----------------------------------------
filterCodes :: Move -> [Code] -> [Code]
filterCodes move codes = filter (\code -> (isConsistent move code)) codes

-- Exercise 6 -----------------------------------------

allCodes :: Int -> [Code]
allCodes 0 = []
allCodes 1 = [[next_code] | next_code <- colors]
allCodes n = [(next_code:prev_codes) | next_code <- colors, prev_codes <- (allCodes (n - 1))]

-- Exercise 7 -----------------------------------------

solve :: Code -> [Move]
solve code = map (\x -> getMove code x) (allCodes (length code))

-- Bonus ----------------------------------------------

fiveGuess :: Code -> [Move]
fiveGuess = undefined
