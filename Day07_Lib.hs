module Lib
    ( advent
    ) where


import Control.Arrow
import Data.List
import Data.List.Split

advent :: IO ()
advent = do
    input <- fmap (lines >>> head >>> splitOn "," >>> map read) $ readFile "input.txt"
    -- Part 1
    print $ minimum $ map (\x -> cost input x) [0..(maximum input)]

    -- Part 2
    print $ minimum $ map (\x -> cost2 input x) [0..(maximum input)]


cost :: [Integer] -> Integer -> Integer
cost i v = sum $ map (\x -> abs (x - v)) i

cost2 :: [Integer] -> Integer -> Integer
cost2 i v = sum $ map (\x -> tri(abs (x - v))) i

tri :: (Integral a) => a -> a
tri n = sum [1..n]
