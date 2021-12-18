module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace
import Control.Monad.State

fileName = "input.txt"
--fileName = "test.txt"

data Snail = Leaf Int | Node Snail Snail deriving (Show, Eq)

advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> map (fst . getSnail)) $ readFile fileName

    -- Part 1
    print $ magnitude $ scrunch input

    -- Part 2
    print $ maximum $ map magnitude $  map scrunch $ [[a,b] | a <- input, b <-input]

scrunch :: [Snail] -> Snail
scrunch (f:s:xs) = scrunch ((reduce $ Node f s):xs)
scrunch (f:[]) = reduce f
scrunch [] = error "no"

reduce :: Snail -> Snail
reduce s = if (exploded /= s) then
               reduce exploded
           else 
               if (splitted /= s) then 
                   reduce splitted
               else s
    where
        exploded = exploder s
        splitted = splitter s


exploder :: Snail -> Snail
exploder s = r
    where
        (r, _, _, _) = explodeInner 0 s
        explodeInner :: Int -> Snail -> (Snail,Bool,Int,Int)
        explodeInner l s@(Leaf n) = (s, False, 0, 0)
        explodeInner l s@(Node (Leaf a) (Leaf b)) = if l >= 4 then (Leaf 0, True, a, b) else (s, False, 0, 0)
        explodeInner l s@(Node a b) = if explodeLeft then (Node lhs (hopLeft leftRight b), True, leftLeft, 0)
                                       else if explodeRight then (Node (hopRight a rightLeft) rhs, True, 0, rightRight)
                                       else (s, False, 0, 0)
                where
                    (lhs,explodeLeft,leftLeft,leftRight) = explodeInner (l+1) a
                    (rhs,explodeRight,rightLeft,rightRight) = explodeInner (l+1) b

hopLeft :: Int -> Snail -> Snail
hopLeft n (Leaf m) = Leaf (n+m)
hopLeft n (Node a b) = Node (hopLeft n a) b

hopRight :: Snail -> Int -> Snail
hopRight (Leaf m) n = Leaf (n+m)
hopRight (Node a b) n = Node a (hopRight b n)

splitter :: Snail -> Snail
splitter (Leaf n) = if n < 10 then Leaf n else Node (Leaf (n `div` 2)) (Leaf ((n+1) `div` 2))
splitter (Node a b) = if (lSplit /= a) then Node lSplit b else (Node a rSplit)
    where
        lSplit = splitter a
        rSplit = splitter b

getSnail :: String -> (Snail,String)
getSnail s@(h:xs)
    | h == '['= (Node left right, rest')
    | otherwise =  (Leaf (digitToInt h), xs)
        where
           (left, (comma:rest)) = getSnail xs
           (right, (_:rest')) = getSnail rest

magnitude :: Snail -> Int
magnitude (Leaf n) = n
magnitude (Node s t) = 3*(magnitude s) + 2*(magnitude t)
