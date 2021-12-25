module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M

-- Grid things
data Cell = DRight | DDown | DEmpty deriving (Show, Eq)
type Grid = M.Map (Int, Int) Cell

fileName = "input.txt"
width = 139
height = 137

--fileName = "test.txt"
--width = 10
--height = 9

allPoints = [(x,y) | y <- [0..(height-1)] , x<-[0..(width-1)]]
printGrid :: Grid -> Int -> IO ()
printGrid g h = mapM_ putStrLn $ map (\m -> flatten $ map show $ map snd $ M.toList $ M.filterWithKey (\(x,y) _ -> y == m) g) [0..h-1]

advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> map parse >>> flatten) $ readFile fileName
    let m = M.fromList (zip allPoints input) :: Grid
    let (f,c,t) = until (\(_,_,t) -> t == 0) step (m,0,1)
    -- Part 1
    print c

step :: (Grid, Int, Int) -> (Grid,Int,Int)
step (g,c,t) = (goDown,c+1, (length rights) + (length downs))
    where
        rights = filter (\(x,y) -> (mapRead g (x,y) == DRight) && (mapRead g (if x == (width-1) then 0 else (x+1),y) == DEmpty)) $ map fst $ M.toList g
        goRight = foldl moveRight g rights
        downs = filter (\(x,y) -> (mapRead goRight (x,y) == DDown) && (mapRead goRight (x, if y == (height-1) then 0 else (y+1)) == DEmpty)) $ map fst $ M.toList goRight
        goDown = foldl moveDown goRight downs

moveRight g (x,y) = g''
    where
        g' = M.insert (x,y) DEmpty g
        g'' = M.insert (if x == (width-1) then 0 else (x+1),y) DRight g'

moveDown g (x,y) = g''
    where
        g' = M.insert (x,y) DEmpty g
        g'' = M.insert (x, if y == (height-1) then 0 else (y+1)) DDown g'

parse :: String -> [Cell]
parse s = map cellMap s
    where
        cellMap 'v' = DDown
        cellMap '>' = DRight
        cellMap '.' = DEmpty

-- Other
flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

mapRead :: Ord a => (M.Map a Cell) -> a -> Cell
mapRead m a = if (M.member a m) then (m M.! a) else DEmpty
