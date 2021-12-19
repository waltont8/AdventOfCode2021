module Lib
    ( advent
    ) where


import Control.Arrow
import qualified Data.Set as Set
import Data.Char
import Data.List
import Data.List.Split
import Debug.Trace

fileName = "input.txt"
--fileName = "test.txt"

-- Types
type Beacon = (Int, Int, Int)
type Scanner = Set.Set Beacon

printScanner :: Scanner -> Int -> IO ()
printScanner s n = do
    putStrLn $ "--- scanner " ++ (show n) ++ " ---"
    mapM_ (putStrLn . show) $ Set.toList s

printScanners :: [Scanner] -> IO ()
printScanners s = mapM_ (uncurry printScanner) $ zip s [0..]

rX (x, y, z) = (x, -z, y)
rY (x, y, z) = (z, y, -x)
rZ (x, y, z) = (y, -x, z)
allDir =
        [ (id)
        , (rX)
        , (rY)
        , (rZ)
        , (rX >>> rX)
        , (rX >>> rY)
        , (rX >>> rZ)
        , (rY >>> rX)
        , (rY >>> rY)
        , (rZ >>> rY)
        , (rZ >>> rZ)
        , (rX >>> rX >>> rX)
        , (rX >>> rX >>> rY)
        , (rX >>> rX >>> rZ)
        , (rX >>> rY >>> rX)
        , (rX >>> rY >>> rY)
        , (rX >>> rZ >>> rZ)
        , (rY >>> rX >>> rX)
        , (rY >>> rY >>> rY)
        , (rZ >>> rZ >>> rZ)
        , (rX >>> rX >>> rX >>> rY)
        , (rX >>> rX >>> rY >>> rX)
        , (rX >>> rY >>> rX >>> rX)
        , (rX >>> rY >>> rY >>> rY)
        ]

getAllRotations' :: [(Beacon -> Beacon)] -> Scanner -> [Scanner]
getAllRotations' [] _ = []
getAllRotations' (h:xs) s2 = (Set.map h s2):(getAllRotations' xs s2)

getAllRotations :: Scanner -> [Scanner]
getAllRotations s = getAllRotations' allDir s

translate :: Scanner -> (Int,Int,Int) -> Scanner
translate s (xo,yo,zo) = Set.map (\(x,y,z) -> (x-xo,y-yo,z-zo)) s

getAllMoves :: Scanner -> Scanner -> [Scanner]
getAllMoves s1 s2 = map (translate s2) allOffsets
    where
        allOffsets = [(l-x,m-y,n-z)|(x,y,z) <- Set.toList s1, (l,m,n) <- Set.toList s2]

advent :: IO ()
advent = do
    -- Basic parse
    ------------------------------------------------------------------------------ vv Part 2 baddness vv -----------------------------------------
    input <- fmap (lines >>> listSplit (=="") >>> map tail >>> map (map parse) >>> (map ((100000,0,0):)) >>> map Set.fromList) $ readFile fileName

    -- Part 1
    let res = shuffleOver (head input) (tail input)
    print $ (length res) {-- part 2 badness >>> --} - (length input)

    -- Part 2
    -- Get back all those 100000s I put in above, correct them back to zero and then...
    let points = map fixit $ filter (\(x,y,z) -> (abs(x) + abs(y) + abs(z)) > 50000) (Set.toList res)
    -- ... find the Manhattan distance
    print $ maximum [abs(x1-x2) + abs(y1-y2) + abs(z1-z2) | (x1, y1, z1)<-points, (x2, y2, z2)<-points]

fixit :: (Int,Int,Int) -> (Int,Int,Int)
fixit (x,y,z)
    | x > 50000 = (x-100000,y,z)
    | x < (negate 50000) = (x+100000,y,z)
    | y > 50000 = (x,y-100000,z)
    | y < (negate 50000) = (x,y+100000,z)
    | z > 50000 = (x,y,z-100000)
    | z < (negate 50000) = (x,y,z+100000)
    | otherwise = error $ show (x,y,z)


shuffleOver :: Scanner -> [Scanner] -> Scanner
shuffleOver s1 [] = s1
shuffleOver s1 rest = shuffleOver newS1 newRest
    where
        (newS1,newRest) = findNextMatch s1 [] rest


findNextMatch :: Scanner -> [Scanner] -> [Scanner] -> (Scanner, [Scanner])
findNextMatch _ _ [] = error "No match"
findNextMatch s1 waste (h:xs) = if result then (Set.union s1 m, waste ++ xs) else findNextMatch s1 (h:waste) xs
    where
        (result, m)  = match s1 h


match :: Scanner -> Scanner -> (Bool, Scanner)
match s1 b = forAllRotations s1 allRotations
    where
        allRotations = getAllRotations b

forAllRotations :: Scanner -> [Scanner] -> (Bool, Scanner)
forAllRotations s [] = (False, Set.empty :: (Set.Set Beacon))
forAllRotations s1 (h:xs) = if success then (success, result) else forAllRotations s1 xs
    where
        (success, result) = forAllMoves s1 allMoves
        allMoves = getAllMoves s1 h


forAllMoves :: Scanner -> [Scanner] -> (Bool, Scanner)
forAllMoves s [] = (False, Set.empty :: (Set.Set Beacon))
forAllMoves s (h:xs) = if count > 11 then (True, h) else forAllMoves s xs
    where
        count = Set.size $ Set.intersection s h



parse :: String -> Beacon
parse s = (x,y,z)
    where
        c = splitOn "," s
        (x:y:z:[]) = map (\sc -> if (head sc) == '-' then negate (read (tail sc)) else read sc) c 

flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

--listSplit (==[]) s
listSplit :: Eq a => ([a] -> Bool) -> [[a]] -> [[[a]]]
listSplit p s = case dropWhile p s of
                [] -> []
                s' -> w : listSplit p s''
                  where (w, s'') = break p s'

