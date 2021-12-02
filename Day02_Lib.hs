module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split

data Direction = Forward | Up | Down deriving (Show, Read, Eq)
data Step = Step Direction Int deriving (Show, Eq)

advent :: IO ()
advent = do
    input <- fmap (lines >>> map parse) $ readFile "input.txt"
    -- Step 1
    print $ foldl navstep (0,0) input
    -- Step 2
    print $ foldl navstep' (0,0,0) input

navstep' :: (Int,Int,Int) -> Step -> (Int,Int,Int)
navstep' (x,y,a) (Step Forward n) = (x+n,y+a*n,a)
navstep' (x,y,a) (Step Down n)  = (x,y,a+n)
navstep' (x,y,a) (Step Up n) = (x,y,a-n)

navstep :: (Int,Int) -> Step -> (Int,Int)
navstep (x,y) (Step Forward n) = (x+n,y)
navstep (x,y) (Step Down n)  = (x,y+n)
navstep (x,y) (Step Up n) = (x,y-n)

parse :: String -> Step
parse s = Step dir dist
    where
        [w,n] = splitOn " " s
        -- Match case for default read
        dir = read $ toUpper (head w) : map toLower (tail w)
        dist = read n
