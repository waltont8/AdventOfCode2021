module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M

fileName = "input.txt"
--fileName = "test.txt"

-- Grid stuff
type Grid = M.Map (Int, Int) Int
printGrid :: Grid -> Int -> IO ()
printGrid g h = mapM_ putStrLn $ map (\m -> flatten $ map show $ map snd $ M.toList $ M.filterWithKey (\(x,y) _ -> y == m) g) [0..h-1]



advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> listSplit (==[])) $ readFile fileName
    let coords = map parse $ input!!0
    let folds = map foldParse $ input!!1

    -- Part 1
    print $ length $ foldIt coords 'x' 655


    -- Part 2
    -- Work
    let allFolded = foldl (\g (c,n) -> foldIt g c n) coords folds
    -- Print
    -- Make an empty grid in a map and add the points
    let height = 1+ (maximum $ map snd allFolded)
    let width = 1+ (maximum $ map fst allFolded)
    let allPoints = [(x,y) | x<-[0..(width-1)], y <- [0..(height-1)]]
    let emptyGrid = foldl (\m (x,y) -> M.insert (x,y) 0 m) (M.empty::Grid) allPoints
    let allg = foldl mapInc emptyGrid allFolded

    printGrid allg height


foldIt :: [(Int,Int)] -> Char -> Int -> [(Int,Int)]
foldIt g 'x' n = ll `union` frl
    where
      ll = filter (\(x,y) -> x < n) g
      rl = filter (\(x,y) -> x > n) g
      frl = map (\(x,y) -> (n+(negate (x-n)),y)) rl

foldIt g 'y' n = ul `union` fdl
    where
      ul = filter (\(x,y) -> y < n) g
      dl = filter (\(x,y) -> y > n) g
      fdl = map (\(x,y) -> (x, n+(negate (y-n)))) dl

parse :: String -> (Int,Int)
parse s = (x,y)
    where
        a = map read (splitOn "," s)
        x = a!!0
        y = a!!1

foldParse :: String -> (Char, Int)
foldParse s = (fx, fn)
    where
        i = drop 11 s
        p = splitOn "=" i
        fx = head i
        fn = read (p!!1)

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

--listSplit (==[]) s
listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'


-- Add to or increment a map
mapInc :: Ord a => (M.Map a Int) -> a -> (M.Map a Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else m

mapAdd :: Ord a => (M.Map a Int) -> a -> Int -> (M.Map a Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

mapRead :: Ord a => (M.Map a Int) -> a -> Int
mapRead m a = if (M.member a m) then (m M.! a) else 0
