module Lib
    ( advent
    ) where

import Control.Arrow
import Data.List
import Data.List.Split

advent :: IO ()
advent = do
    input <- fmap (lines >>> map (splitOn ",") >>> head >>> map read) $ readFile "input.txt"
    let bucket = zipWith (\n i -> (length $ filter (==n) i)) [0..8] (repeat input)
    print $ sum $ last $ take (80+1) $ iterate step bucket
    print $ sum $ last $ take (256+1) $ iterate step bucket


step :: [Int] -> [Int]
step i = [i!!1, i!!2, i!!3, i!!4, i!!5, i!!6, i!!7+i!!0, i!!8, i!!0]
