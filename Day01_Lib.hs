module Lib
    ( advent
    ) where

import Control.Arrow
import Data.List

advent :: IO ()
advent = do
        -- Part 1
        input <- fmap (lines >>> map (read::String->Int)) $ readFile "input.txt"
        print $ sum $ map fromEnum $ zipWith (<) input (tail input)

        -- Part 2
        let reduceNoise = map sum $ transpose $ [input, tail input, (tail . tail) input]
        print $ sum $ map fromEnum $ zipWith (<) reduceNoise (tail reduceNoise)
