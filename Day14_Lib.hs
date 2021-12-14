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


advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> listSplit (==[])) $ readFile fileName
    let startVal = head (input!!0)
    let rules = M.fromList $ map parse (input!!1)

    -- Part 1
    let res = last $ take (10+1) $ iterate (simulate rules) startVal
    print $ (sort >>> group >>> map length >>> sort >>> (head &&& last) >>> uncurry (-) >>> negate) res

    -- Part 2
    let pairDB = foldl (\m p -> mapInc m p) (M.empty :: (M.Map String Int)) $ map (\(a,b) -> (a:b:[])) $ zip startVal (tail startVal)
    let res = last $ take (40+1) $ iterate (simulate2 rules) pairDB

    let starts = foldl (\m (s,n) -> mapAdd m s n) (M.empty :: (M.Map String Int)) $ map (\(s,n) -> ([s!!0],n)) $ (M.toList res)
    let ends = foldl (\m (s,n) -> mapAdd m s n) (M.empty :: (M.Map String Int)) $ map (\(s,n) -> ([s!!1],n)) $ (M.toList res)
    let singles = zipWith (\(s1,n1) (s2,n2) -> if n1 > n2 then (s1,n1) else (s2,n2)) (M.toList starts) (M.toList ends)

    print $ (sort >>> (head &&& last) >>> uncurry (-) >>> negate) $ map snd singles



simulate2 :: (M.Map String String) -> (M.Map String Int) -> (M.Map String Int)
simulate2 m i = foldl (\oPear (s,n) -> updateMap m oPear s n) i (M.toList i)

updateMap :: (M.Map String String) -> (M.Map String Int) -> String -> Int -> (M.Map String Int)
updateMap m i s n = M.filter (>0) i'''
    where
        ins = mapRead m s
        i' = mapSub i s n
        i'' = mapAdd i' ([s!!0]++ins) n
        i''' = mapAdd i'' (ins ++ [s!!1]) n


simulate :: (M.Map String String) -> String -> String
simulate rules startVal = (foldl (\s pr -> s ++ [head pr] ++ (mapRead rules pr)) "" pears) ++ [last startVal]
    where
        pears = map (\(a,b) -> (a:b:[])) $ zip startVal (tail startVal)

parse :: String -> (String, String)
parse s = (a!!0,a!!1)
    where
        a = splitOn " -> " s

--listSplit (==[]) s
listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'


mapInc :: (M.Map String Int) -> String -> (M.Map String Int)
mapInc m a = if (M.member a m) then (M.insert a ((m M.! a) + 1) m) else (M.insert a 1 m)

mapDec :: (M.Map String Int) -> String -> (M.Map String Int)
mapDec m a = if (M.member a m) then (M.insert a ((m M.! a) - 1) m) else (M.insert a (negate 1) m)

mapAdd :: (M.Map String Int) -> String -> Int -> (M.Map String Int)
mapAdd m a i = if (M.member a m) then (M.insert a ((m M.! a) + i) m) else (M.insert a i m)

mapSub :: (M.Map String Int) -> String -> Int -> (M.Map String Int)
mapSub m a i = if (M.member a m) then (M.insert a ((m M.! a) - i) m) else (M.insert a (negate i) m)

mapRead :: (M.Map String String) -> String -> String
mapRead m a = if (M.member a m) then (m M.! a) else ""
