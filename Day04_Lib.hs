module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.Ord
import Data.List.Split

advent :: IO ()
advent = do
    input <- fmap lines $ readFile "input.txt"
    -- Get calls
    let calls = (head >>> splitOn "," >>> map read) input

    -- Parse out boards
    let boards = parse (drop 1 input)

    -- Play all games to conclusion
    let results = sortBy (comparing fst) $ map (simulate 0 calls) boards

    let (t,(lastCall,lastBoard)) = last $ results
    let (t,(firstCall,firstBoard)) = head $ results

    -- Part 1
    print $ sum ((filter (>=0)) (concat firstBoard)) * firstCall

    -- Part 2
    print $ sum ((filter (>=0)) (concat lastBoard)) * lastCall


parse :: [String] -> [[[Int]]]
parse [] = []
parse (s:a:b:c:d:e:xs) = [map read (words a),
                                 map read (words b),
                                 map read (words c),
                                 map read (words d),
                                 map read (words e)] : parse xs

simulate :: Int -> [Int] -> [[Int]] -> (Int, (Int, [[Int]]))
simulate c [] b = (c, (-2, b))
simulate c (h:xs) b  
    | winning nb = (c, (h, nb))
    | winning (transpose nb) = (c, (h, nb))
    | otherwise  = simulate (c+1) xs nb
        where
            nb = map (replace h (-1)) b

-- Check for horizontal wins, call with transpose for vertical
winning :: [[Int]] -> Bool            
winning i@[a,b,c,d,e]
    | all (==(-1)) a = True
    | all (==(-1)) b = True
    | all (==(-1)) c = True
    | all (==(-1)) d = True
    | all (==(-1)) e = True
    | otherwise = False

replace :: Int -> Int -> [Int] -> [Int]
replace _ _ [] = []
replace n o (h:xs) = (if (h == n) then o else h) : (replace n o xs)
