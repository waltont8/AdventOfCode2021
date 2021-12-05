module Lib
    ( advent
    ) where


import Control.Arrow
import Data.List
import Data.List.Split
import qualified Data.Map as M

advent :: IO ()
advent = do
    input <- fmap (lines >>> map parse) $ readFile "input.txt"
    -- Filter H and V lines only
    let p1data = filter (\((x1,y1),(x2,y2)) -> x1 == x2 || y1 == y2) input
    -- Collect points for part 1
    let points = foldl pointRecorder (M.empty :: M.Map (Int,Int) Int) p1data
    -- Part 1
    print $ M.size $ M.filter (>1) points

    -- Collect points for part 2, all data
    let points2 = foldl pointRecorder (M.empty :: M.Map (Int,Int) Int) input
    -- Part 2
    print $ M.size $ M.filter (>1) points2

parse :: String -> ((Int,Int),(Int,Int))
parse s = ((read x1, read y1),(read x2, read y2))
    where
      [a,b] = splitOn " -> " s
      [x1,y1] = splitOn "," a
      [x2,y2] = splitOn "," b

pointRecorder :: (M.Map (Int,Int) Int) -> ((Int,Int), (Int,Int)) -> (M.Map (Int,Int) Int)
pointRecorder pm p = foldl mapInc pm (toPoints p)


-- Step along the lines, collecting up the points
toPoints :: ((Int,Int),(Int,Int)) -> [(Int,Int)]
toPoints ((x1,y1),(x2,y2))
    | x1 < x2 && y1 < y2 = (x1,y1) : toPoints ((x1+1,y1+1),(x2,y2))
    | x1 > x2 && y1 > y2 = (x1,y1) : toPoints ((x1-1,y1-1),(x2,y2))
    | x1 < x2 && y1 > y2 = (x1,y1) : toPoints ((x1+1,y1-1),(x2,y2))
    | x1 > x2 && y1 < y2 = (x1,y1) : toPoints ((x1-1,y1+1),(x2,y2))
    | x1 < x2 = (x1,y1) : toPoints ((x1+1,y1),(x2,y2))
    | x1 > x2 = (x1,y1) : toPoints ((x1-1,y1),(x2,y2))
    | y1 < y2 = (x1,y1) : toPoints ((x1,y1+1),(x2,y2))
    | y1 > y2 = (x1,y1) : toPoints ((x1,y1-1),(x2,y2))
    | x1 == x2 && y1 == y2 = [(x1,y1)]


-- Add to or increment a map
mapInc :: Ord a => (M.Map a Int) -> a -> (M.Map a Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else (M.insert a 1 m)

mapAdd :: Ord a => (M.Map a Int) -> a -> Int -> (M.Map a Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

mapRead :: Ord a => (M.Map a Int) -> a -> Int
mapRead m a = if (M.member a m) then (m M.! a) else 0
