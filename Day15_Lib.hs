module Lib
    ( advent
    ) where

import Data.Char
import Data.Maybe
import Algorithm.Search (pruning, dijkstra)


-- Part 1
--width = 100
--height = 100
--tileWidth = 1
--tileHeight = 1

-- part 2
width = 100
height = 100
tileWidth = 5
tileHeight = 5
start = (0,0)
end = (tileWidth*width-1,tileHeight*height-1)

advent :: IO ()
advent = do
    -- Part 1
    let (cost, path) = fromJust $ dijkstra (neighbors `pruning` isWall) dist (==end) (start) 
    let cPath = map snd path
    print cost


neighbors (x, y) = [(x, y + 1), (x - 1, y), (x + 1, y), (x, y - 1)]

isWall ((x,y))
    | x < 0 = True
    | y < 0 = True
    | x >= (tileWidth*width) = True
    | y >= (tileHeight*height) = True
    | otherwise = False

dist :: (Int,Int) -> (Int,Int) -> Int
dist (x1,y1) (x2,y2) = (d + ((x2 `div` width) + (y2 `div` height))) `oddmod` 9 
    where
        -- Wrap to 1 for part 2??
        oddmod a b = if (a > b) then (a-b) else a

        d = digitToInt $ m!!((x2 `mod` width) +((y2 `mod` height)*width))
        -- input data as a string
        m = "68993... *removed before upload* ...98559981949"
