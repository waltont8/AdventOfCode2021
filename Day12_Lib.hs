module Lib
    ( advent
    ) where


import Control.Arrow
import qualified Data.Set as Set
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M

fileName = "input.txt"
--fileName = "test.txt"


type Graph = M.Map String (Int,[String])


advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> map parse) $ readFile fileName
    let g = foldl (addToGraph) (M.empty :: Graph) input

    -- Part 1
    print $ length $ countPaths g "start" "start"

    -- Part 2
    -- just make all possible graphs with different small letters as 2
    let megaG = map (\(name,(cnt,lst)) -> M.insert name (cnt+1,lst) g) $ filter (\(name,(cnt,lst)) -> cnt == 1 && name /= "start") (M.toList g)
    -- Walk all of them, merge them together. Set union is faster than list union.
    print $ Set.size $ foldl (Set.union) ((Set.empty)::(Set.Set String)) $ map (\ig -> Set.fromList $ countPaths ig "start" "start") megaG

-- Add both directions to the Graph structure
addToGraph :: Graph -> (String, String) -> Graph
addToGraph g (a,b) = o
    where
        g' = mapAddArray g a b
        o = mapAddArray g' b a


parse :: String -> (String, String)
parse s = (a,b)
    where
        [a,b] = splitOn "-" s


countPaths :: Graph -> String -> String -> [String]
countPaths g "end" dbg = [dbg]
countPaths g cur dbg = if (seen == 0) then [] else res
    where
        ng = markVisitedIfLower g cur
        (seen, exits) = g M.! cur
        res = flatten $ map (\s -> countPaths ng s (dbg ++ "," ++ s)) exits

markVisitedIfLower :: Graph -> String -> Graph
markVisitedIfLower g n = M.insert n (c-1,l) g
    where
        (c,l) = g M.! n


flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

mapRead m a 
    | (M.member (a,False) m) = (m M.! (a,False))
    | (M.member (a,True) m) = (m M.! (a, True))
    | otherwise = []

mapAddArray m a b = if (M.member a m) then (M.insert a (c,(b:ol)) m) else (M.insert a (c,[b]) m)
    where
        c = getCount a
        (oc, ol) = if (M.member a m) then m M.! a else (0,[])

getCount s
    | isLower (head s) = 1
    | otherwise = -1
