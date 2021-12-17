module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split

type Point = (Int,Int)
type Area = (Point,Point)
type Velocity = (Int,Int)

advent :: IO ()
advent = do
    -- Basic parse
    let iA = ((143,negate 106),(177,negate 71))
    -- let tA = ((20,0-10),(30,0-5))


    -- Part 1
    let allV = [(x,y) | x <- [0..178], y<-[(negate 177)..177]] -- guess range
    let results = filter (\((p,b),_) -> b) $ map (\(ix,iy) -> (simulate (0,0) (ix,iy) iA, (ix,iy))) allV
    let highest = maximum $ flatten $ map (map (\(x,y) -> y)) $ map (\((p,b),_) -> p) $ results

    print highest

    -- Part 2
    print $ length results

simulate :: Point -> Velocity -> Area -> ([Point], Bool)
simulate p@(x,y) v@(vx,vy) a@((x1,y1),(x2,y2))
    | inArea p a = ([],True)
    | (x > x2  || (y<y1 && vy<0)) = ([],False)
    | otherwise = (\(l,b) -> (p:l,b)) $ simulate np nv a
      where
        (np,nv) = step p v
        hit = inArea p a


inArea :: Point -> Area -> Bool
inArea (x,y) ((x1,y1),(x2,y2)) = if ((x>=x1) && (x<=x2) && (y>=y1) && (y<=y2)) then True else False

step :: Point -> Velocity -> (Point, Velocity)
step (x,y) (vx,vy) = ((x+vx,y+vy) , (absDec vx, vy-1))
    where
        absDec a | a<0 = a+1
                 | a>0 = a-1
                 | a==0 = 0

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []
