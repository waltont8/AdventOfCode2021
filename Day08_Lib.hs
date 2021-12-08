module Lib
    ( advent
    ) where


import Control.Arrow
import Data.List
import Data.List.Split
import Data.Maybe


advent :: IO ()
advent = do
    input <- fmap (lines >>> map parse ) $ readFile "input.txt"
    -- Part 1
    print $ length $ filter (\x -> x `elem` [2,4,3,7]) $ (map snd >>> (map (map length)) >>> flatten) input

    -- Part 2
    print $ sum $ map decode input

decode (a,b) = (fromMaybe 0 (justs!!3)) + (fromMaybe 0 (justs!!2))*10 + (fromMaybe 0 (justs!!1))*100 + (fromMaybe 0 (justs!!0))*1000
    where
        digs = map sort (reason a)
        res = map sort b
        justs = map ((flip elemIndex) digs) res


reason :: [String] -> [String]
reason h =  [zero,one,two,three,four,five,six,seven,eight,nine]
    where
        one = getN h 2
        four = getN h 4
        seven = getN h 3
        eight = getN h 7
        three = head $ filter (\s -> hasAll one s) $ filter (\s -> length s == 5) h
        five = head $ filter (\s -> hasAll ((\\) four one) s) $ filter (\s -> length s == 5) h
        two = head $ filter (\s -> (s /= three) && (s /= five)) $ filter (\s -> length s == 5) h
        nine = head $ filter (\s -> hasNotAll ((\\) ((\\) eight four) three) s) $ filter (\s -> length s == 6) h
        zero = head $ filter (\s -> hasAll one s) $ filter (\s -> s /= nine) $ filter (\s -> length s == 6) h
        six = head $ filter (\s -> s /= zero ) $ filter (\s -> s /= nine) $ filter (\s -> length s == 6) h



flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

hasAll :: String -> String -> Bool
hasAll a b = all (`elem` b) a

hasNotAll :: String -> String -> Bool
hasNotAll a b = False == (all (`elem` b) a)

parse :: String -> ([String],[String])
parse s = (splitOn "|" >>> ((head >>> words) &&& (last >>> words))) s

getN :: [String] -> Int -> String
getN i n = snd $ head $ filter (\(c,s) -> c == n) $ zip (map length i) i
