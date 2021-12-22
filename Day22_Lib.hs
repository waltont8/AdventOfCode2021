module Lib
    ( advent
    ) where


import Control.Arrow
import qualified Data.Set as Set
import Data.List
import Data.List.Split

fileName = "input.txt"
--fileName = "test.txt"

type Range = (Integer, Integer)
type Cube = (Range, Range, Range)
type Inst = (Bool, Cube)


advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> map parse) $ readFile fileName

    -- Part 1
    let res = foldl (\s (o,((x1,x2),(y1,y2),(z1,z2))) -> updateSet o s (Set.fromList [(x,y,z) | x<-[x1..x2], y<-[y1..y2], z<-[z1..z2]]) ) Set.empty (take 20 input)
    print $ show $ Set.size res

    -- Part 2
    --print $ input
    let res = foldl addCube [] input
    print $ sum $ map volume $ res

parse :: String -> Inst
parse s = (onoff, ((ranges!!2, ranges!!4),(ranges!!7,ranges!!9),(ranges!!12,ranges!!14)))
    where
        onoff = if (head $ words $ s) == "on" then True else False
        ranges = map readInteger $ splitOneOf "xyz=,.." (last $ words $ s)

-- Part 1
updateSet o s1 s2
    | o = Set.union s1 s2
    | not o = Set.difference s1 s2

-- Part 2
rangeOverlaps (x1_a,x2_a) (x1_b,x2_b) 
    | (x1_a >= x1_b && x1_a <= x2_b) || (x2_a <= x2_b && x2_a >= x1_b) = True
    | (x1_b >= x1_a && x1_b <= x2_a) || (x2_b <= x2_a && x2_b >= x1_a) = True
    | otherwise = False

volume :: (Bool, Cube) -> Integer
volume (o,((a,b),(c,d),(e,f))) = (b-a+1)*(d-c+1)*(f-e+1)*(if o then 1 else (negate 1))

overlap :: Cube -> Cube -> Bool
overlap (rx1,ry1,rz1) (rx2,ry2,rz2) = if (rangeOverlaps rx1 rx2) && (rangeOverlaps ry1 ry2) && (rangeOverlaps rz1 rz2) then True else False

addCube db i@(onoff,(rx,ry,rz)) 
    -- On gets added
    | onoff = i : dba
    -- Off does not
    | not onoff = dba
        where
            dba = flatten $ map (cutHole i) db

cutHole (oa,(xa,ya,za)) (o,(x,y,z)) = if overlap ((xa,ya,za)) ((x,y,z)) then
                        [(o,(xr,yr,zr)) | xr <- splitARange x xa 
                                           ,yr <- splitARange y ya 
                                           , zr <- splitARange z za
                        ,not $ and [ rangeOverlaps xr xa, rangeOverlaps yr ya, rangeOverlaps zr za]]
                        else [(o,(x,y,z))]
            where
                splitARange (x1split, x2split) (x1, x2) 
                        | x1split < x1  = (x1split, x1-1) : splitARange (x1, x2split) (x1, x2)
                        | x2split > x2  = (x2+1, x2split) : splitARange (x1split, x2) (x1, x2)
                        | otherwise = [(x1split, x2split)]


flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []


readInteger :: String -> Integer
readInteger "" = 0
readInteger (h:xs) = if h =='-' then negate (read xs) else (read (h:xs))
