{-# OPTIONS_GHC -Wall #-}
module HW06 where

import Data.List
import Data.Functor

-- Exercise 1 -----------------------------------------

fib :: Integer -> Integer
fib 0 = 1
fib 1 = 1
fib x = (fib (x - 1)) + (fib (x - 2))

fibs1 :: [Integer]
fibs1 = 1 : 1 : zipWith (+) fibs1 (tail fibs1)

-- Exercise 2 -----------------------------------------

fibs2 :: [Integer]
fibs2 = 1 : 1 : zipWith (+) fibs1 (tail fibs1)

fibs3 :: [Integer]
fibs3 = 1 : 1 : (map sum (tail (tail (inits fibs3))))

-- Exercise 3 -----------------------------------------

data Stream a = Cons a (Stream a)

-- Show instance prints the first 20 elements followed by ellipsis
instance Show a => Show (Stream a) where
    show s = "[" ++ intercalate ", " (map show $ take 10 $ streamToList s)
             ++ ",..."

streamToList :: Stream a -> [a]
streamToList (Cons a s) = a : (streamToList s)

-- Exercise 4 -----------------------------------------

instance Functor Stream where
    fmap f (Cons a x) = Cons (f a) (fmap f x)

-- Exercise 5 -----------------------------------------

sRepeat :: a -> Stream a
sRepeat x = Cons x (sRepeat x)

sIterate :: (a -> a) -> a -> Stream a
sIterate f x = Cons x (sIterate f (f x))

sInterleave :: Stream a -> Stream a -> Stream a
sInterleave (Cons b x) (Cons a y) = Cons b (Cons a (sInterleave x y))


sTake :: Int -> Stream a -> [a]
sTake 0 _ = []
sTake n (Cons a s) = a : (sTake (n -1) s)

-- Exercise 6 -----------------------------------------
sSeries :: Integer -> Stream Integer
sSeries x = Cons x (sSeries (x + 1))


nats :: Stream Integer
nats = sSeries 0

ruler :: Stream Integer
ruler = undefined

-- Exercise 7 -----------------------------------------

-- | Implementation of C rand
rand :: Int -> Stream Int
rand = undefined

-- Exercise 8 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMaxSlow :: [Int] -> Maybe (Int, Int)
minMaxSlow [] = Nothing   -- no min or max if there are no elements
minMaxSlow xs = Just (minimum xs, maximum xs)

-- Exercise 9 -----------------------------------------

{- Total Memory in use: ??? MB -}
minMax :: [Int] -> Maybe (Int, Int)
minMax = undefined

main :: IO ()
main = print $ minMaxSlow $ sTake 1000000 $ rand 7666532

-- Exercise 10 ----------------------------------------

fastFib :: Int -> Integer
fastFib = undefined
