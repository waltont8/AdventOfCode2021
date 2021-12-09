module Lib
    ( advent
    ) where


import Control.Arrow
import qualified Data.Set as Set
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

data Direction = Forward | Up | Down deriving (Show, Eq)
data Row = Row Direction Int deriving (Show, Eq)

width = 100
height = 100

advent :: IO ()
advent = do
    input <- fmap (lines >>> map parse >>> flatten ) $ readFile "input.txt"

    -- Coordinate -> height
    let m = foldl (\m (n,i) -> mapAdd m (n `mod` width, n `div` width) i) (M.empty :: M.Map (Int,Int) Int) $ zip [0..] input

    -- Part 1
    let lows = filter (\((x,y), h) -> lowerThan ((x,y),m)) (M.toList m)
    let res = (sum (map snd lows)) + (length lows)
    print res

    -- Part 2
    let basins = foldl (\l ((x,y),_) -> (fst $ countBasin (x,y) (negate 1) m):l) [] lows
    print $ product $ take 3 $ reverse $ sort basins


parse :: String -> [Int]
parse s = map digitToInt s

countBasin :: (Int, Int) -> Int -> (M.Map (Int,Int) Int) -> (Int, (M.Map (Int,Int) Int))
countBasin (x,y) o m
    | o >= cur = (0, m)
    | cur >= 9 = (0, m)
    | otherwise = (a+b+c+d+1,md)
        where
            cur = (getHeight m (x,y))
            killed = (M.insert (x,y) 11 m)
            (a,ma) = countBasin (x+1,y) cur killed
            (b,mb) = countBasin (x-1,y) cur ma
            (c,mc) = countBasin (x,y+1) cur mb
            (d,md) = countBasin (x,y-1) cur mc


flatten :: [[a]] -> [a]
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

lowerThan ((x,y),m) = (h<a) && (h<b) && (h<c) && (h<d)
    where
        h = getHeight m (x,y)
        a = getHeight m (x,y-1)
        b = getHeight m (x,y+1)
        c = getHeight m (x-1,y)
        d = getHeight m (x+1,y)


mapInc :: Ord a => (M.Map a Int) -> a -> (M.Map a Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else (M.insert a 1 m)

mapAdd :: Ord a => (M.Map a Int) -> a -> Int -> (M.Map a Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

getHeight :: Ord a => (M.Map a Int) -> a -> Int
getHeight m a = if (M.member a m) then (m M.! a) else 10
