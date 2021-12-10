module Lib
    ( advent
    ) where


import Control.Arrow
import Data.List
import Data.List.Split

advent :: IO ()
advent = do
    input <- fmap (lines >>> map (parse [])) $ readFile "input.txt"
    print $ (filter (\(a,b) -> length a > 0) >>> map (\(a,b) -> (head a,head b)) >>> map fst >>> map p1Val >>> sum) input
    print $ (filter (\(a,b) -> length a == 0) >>> map snd >>> map (map rev) >>> map (score 0) >>> sort >>> middle >>> head) input

parse :: String -> String -> (String,String)
parse s [] = ([],s)
parse s (h:xs)
    | h `elem` "[({<" = parse (h:s) xs
    | h `elem` "]}>)" && (rev h) == (head s) = parse (tail s) xs
    | h `elem` "]}>)" = ((h:xs),s)

p1Val :: Char -> Int
p1Val ')' = 3
p1Val ']' = 57
p1Val '}' = 1197
p1Val '>' = 25137

p2Val :: Char -> Int
p2Val ')' = 1
p2Val ']' = 2
p2Val '}' = 3
p2Val '>' = 4

score :: Int -> String -> Int
score n [] = n
score n (h:xs) = score (n*5  + (p2Val h)) xs

rev :: Char -> Char
rev '(' = ')'
rev '[' = ']'
rev '{' = '}'
rev '<' = '>'
rev ')' = '('
rev ']' = '['
rev '}' = '{'
rev '>' = '<'

middle :: [a] -> [a]
middle l@(_:_:_:_) = middle $ tail $ init l
middle l           = l

