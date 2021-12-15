module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Control.Monad.State

width = 100
height = 100

type DM = M.Map (Int,Int) Int

advent :: IO ()
advent = do
    input <- fmap (lines >>> map parse >>> flatten ) $ readFile "input.txt"

    -- Coordinate -> height
    let m = (foldl (\m (n,i) -> mapAdd m (n `mod` width, n `div` width) i) (M.empty :: DM) $ zip [0..] input) :: DM

    -- Part 1
    let lows = filter (\((x,y), h) -> lowerThan ((x,y),m)) (M.toList m)
    let res = (sum (map snd lows)) + (length lows)
    print res

    -- Part 2
    let basins = map (basinCounter m) lows
    print $ product $ take 3 $ reverse $ sort basins


    
basinCounter :: DM -> ((Int,Int),Int) -> Int
basinCounter m ((x,y),_) = evalState (newCount (negate 1) (x,y)) m



newCount :: Int -> (Int, Int) -> State DM Int
newCount ov (x,y) = do
    dm <- get
    let nv = getHeight dm (x,y)
    if (ov >= nv || nv >= 9) then 
        return 0 
    else 
        action ov (x,y)

        where
            action ov (x,y) = do
                dm <- get
                let nv = getHeight dm (x,y)
                put $ setHeight dm (x,y) 10
                liftM (+1) (liftM sum $ (mapM (newCount nv) [(x,y-1),(x,y+1),(x+1,y),(x-1,y)]))


parse :: String -> [Int]
parse s = map digitToInt s


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

mapAdd :: DM -> (Int, Int) -> Int -> DM
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

getHeight :: DM -> (Int,Int) -> Int
getHeight m a = if (M.member a m) then (m M.! a) else 10

setHeight :: DM -> (Int,Int) -> Int -> DM
setHeight m a v = M.insert a v m
