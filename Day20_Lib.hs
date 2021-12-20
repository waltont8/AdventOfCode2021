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

fileName = "input.txt"
--fileName = "test.txt"

-- Grid things
type Grid = M.Map (Int, Int) Char
type Algo = [Char]
type Image = (Grid, Int, Int, Int, Int)


printGrid :: Grid -> IO ()
printGrid g = mapM_ (putStrLn . show) $ map (\y -> map (\x -> mapRead g (x,y)) [lx..hx]) [ly..hy]
    where
        (lx,hx,ly,hy) = gridExtents g

gridExtents :: Grid -> (Int,Int,Int,Int)
gridExtents g = M.foldrWithKey (\(x,y) _ (l,r,t,b) -> (minimum [x,l], maximum [x,r], minimum [t,y], maximum [b,y])) (0,0,0,0) g

advent :: IO ()
advent = do
    -- Basic parse
    [input1, input2] <- fmap (lines >>> listSplit (=="")) $ readFile fileName
    let algo = (flatten input1)::Algo
    let g = (parse input2)::Grid
    let (l,r,t,b) = gridExtents g
    let i = (g,l,r,t,b)

    -- Part 1
    let (res,_,_,_,_) = last $ take (2+1) $ iterate (step algo) i
    print $ M.size $ res

    -- Part 2
    let (res',_,_,_,_) = last $ take (50+1) $ iterate (step algo) i
    print $ M.size $ res'

parse :: [String] -> Grid
parse input = foldl (\m (n,i) -> mapAdd m (n `mod` width, n `div` width) i) (M.empty :: Grid) $ zip [0..] (flatten input)
    where width = length (input!!0)


step :: Algo -> Image -> Image
step a i@(g,l,r,t,b) = (M.fromList onlyHash,l-1,r+1,t-1,b+1)
    where
        allPoints = [(x,y) | x <- [(l-1)..(r+1)], y <-[(t-1)..(b+1)]]
        allNewPixels = zip allPoints (map (getNewPixel i a) allPoints)
        onlyHash = filter (\((x,y), v) -> v == '#') allNewPixels


getNewPixel :: Image -> Algo -> (Int,Int) -> Char
getNewPixel i@(g,l,r,t,b) a (x,y) = newPix
    where
        raw = (getPixel i (x-1,y-1)):(getPixel i (x,y-1)):(getPixel i (x+1,y-1))
             :(getPixel i (x-1,y))  :(getPixel i (x,y))  :(getPixel i (x+1,y))
             :(getPixel i (x-1,y+1)):(getPixel i (x,y+1)):(getPixel i (x+1,y+1)):[]
        n = binToDec raw
        newPix = (a!!n)

-- Other
binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + (if x=='#' then 1 else 0)) 0

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

--listSplit (==[]) s
listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'

mapAdd :: Grid -> (Int,Int) -> Char -> Grid
mapAdd m a i = if i == '#' then M.insert a i m else m

-- Because the first character of the algorithm is a #, the blank infinite space will flash
-- Grrrr!
getPixel i@(g,l,r,t,b) (x,y) = if (x<l ||  x>r || y<t || y>b) then def else nondef
    where
        nondef = mapRead g (x,y)
        def = if (odd r) then '.' else '#'

mapRead :: Grid -> (Int,Int) -> Char
mapRead m a = if (M.member a m) then (m M.! a) else '.'
