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
width = 10
height = 10

allPoints = [(x,y) | x<-[0..(width-1)], y <- [0..(height-1)]]

type Grid = M.Map (Int, Int) Int
advent :: IO ()
advent = do
    -- Basic parse to grid
    input <- fmap (lines >>> map (map digitToInt) >>> flatten) $ readFile fileName
    let m = foldl (\m (n,i) -> mapAdd m (n `mod` width, n `div` width) i) (M.empty :: M.Map (Int,Int) Int) $ zip [0..] input

    -- Part 1
    let (res,n,f) = last $ take 101 $ iterate simulate (m,0,0) 
    printGrid res height
    print f

    -- Part 2
    let (res,n,f) = until (\(a,_,_) -> (sum $ map snd $ M.toList a) == 0) simulate (m,0,0) 
    printGrid res height
    print n

printGrid :: Grid -> Int -> IO ()
printGrid g h = mapM_ putStrLn $ map (\m -> flatten $ map show $ map snd $ M.toList $ M.filterWithKey (\(x,y) _ -> y == m) g) [0..h-1]

simulate :: (Grid,Int,Int) -> (Grid,Int,Int)
simulate (g,countit,fc) = (resetToZero n,countit+1,nfc)
    where
        incd = foldl (mapInc) g allPoints
        (n,nfc) = until (\(a,_) -> M.size (M.filter (>9) a) == 0) flashAll (incd,fc)

resetToZero :: Grid -> Grid
resetToZero m = foldl (\a (x,y) -> if (mapRead a (x,y)) < 0 then M.insert (x,y) 0 a else a) m allPoints

flashAll :: (Grid,Int) -> (Grid,Int)
flashAll (g,n) = ((foldl flash g $ M.toList nines), n + (M.size nines))
    where
        nines = M.filter (>9) g

flash :: Grid -> ((Int,Int), Int) -> Grid
flash inG ((x,y),_) = i
    where
        a = M.insert (x,y) (negate 1000000) inG -- 1000000 is infinity, right?
        b = mapInc a (x+1,y) -- right
        c = mapInc b (x-1,y) -- left
        d = mapInc c (x,y+1) -- down
        e = mapInc d (x,y-1) -- up
        f = mapInc e (x-1,y-1) -- up left
        g = mapInc f (x-1,y+1) -- down left
        h = mapInc g (x+1,y-1) -- up right
        i = mapInc h (x+1,y+1) -- down right

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

-- Add to or increment a map
mapInc :: Ord a => (M.Map a Int) -> a -> (M.Map a Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else m

mapAdd :: Ord a => (M.Map a Int) -> a -> Int -> (M.Map a Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

mapRead :: Ord a => (M.Map a Int) -> a -> Int
mapRead m a = if (M.member a m) then (m M.! a) else 0
