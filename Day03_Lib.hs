module Lib
    ( advent
    ) where


import Control.Arrow
import Data.List
import Data.Char

advent :: IO ()
advent = do
    input <- fmap lines $ readFile "input.txt"
    -- part 1
    let mostCommon = map (sort >>> group >>> map length) $ transpose input
    let gammaRate = toDec $ foldl (\r [z,o] -> r++[(if z>o then '0' else '1')]) "" mostCommon
    let epsilonRate = toDec $ foldl (\r [z,o] -> r++[(if z<o then '0' else '1')]) "" mostCommon
    print $ (show gammaRate) ++ "*" ++ (show epsilonRate) ++ "=" ++ (show $gammaRate*epsilonRate)

    -- part 2
    let o = toDec $ oxygen input 0
    let c = toDec $ carbon input 0
    print $ (show o) ++ "*" ++ (show c) ++ "=" ++ (show $c*o)


oxygen (h:[]) _ = h
oxygen s n
        | o > z = oxygen (filter (\(s) -> (s!!n) == '1') s) (n+1)
        | z > o = oxygen (filter (\(s) -> (s!!n) == '0') s) (n+1)
        | z == o = oxygen (filter (\(s) -> (s!!n) == '1') s) (n+1)
            where
               pops = map ((filter (=='0') >>> length) &&& (filter(=='1') >>> length)) $ transpose s
               (z,o) =  pops !! n


carbon (h:[]) _ = h
carbon s n
        | o < z = carbon (filter (\(s) -> (s!!n) == '1') s) (n+1)
        | z < o = carbon (filter (\(s) -> (s!!n) == '0') s) (n+1)
        | z == o = carbon (filter (\(s) -> (s!!n) == '0') s) (n+1)
            where
               pops = map ((filter (=='0') >>> length) &&& (filter(=='1') >>> length)) $ transpose s
               (z,o) =  pops !! n


toDec :: String -> Int
toDec = foldl' (\acc x -> acc * 2 + digitToInt x) 0







