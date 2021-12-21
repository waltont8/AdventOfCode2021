module Lib
    ( advent
    ) where


import Control.Arrow
import Control.Monad
import Control.Monad.State
import Data.List
import qualified Data.Map as M
import Data.Tuple
import Debug.Trace



type Dice = Int
type Turn = Int
data PState = PState { score::Int, position::Int } deriving Show
type PState2 = ((Int, Int), (Int, Int))

data GS = GS PState PState Bool Int Dice deriving Show

step :: GS -> GS
step g@(GS p1@(PState p1s p1p) p2@(PState p2s p2p) turn rollCount d) = GS np1 np2 (not turn) (rollCount+3) nd
    where
        d1 = d
        d2 = roll d1
        d3 = roll d2
        rollVal = d1+d2+d3
        nd = roll d3

        np1p = move p1p rollVal
        np1s = p1s + np1p

        np2p = move p2p rollVal
        np2s = p2s + np2p

        np1 = if turn then (PState np1s np1p) else p1
        np2 = if not turn then (PState np2s np2p) else p2

roll n = if n == 100 then 1 else n+1
move n pos = ((pos + n - 1) `mod` 10) + 1


advent :: IO ()
advent = do
    -- Basic parse
    let start = GS (PState 0 7) (PState 0 5) True 0 1
    --let start = GS (PState 0 4) (PState 0 8) True 0 1

    -- Part 1
    let res = until (\g@(GS (PState p1s p1p) (PState p2s p2p) _ t d) -> p2s >= 1000 || p1s >= 1000) step start
    let (GS (PState p1s _) (PState p2s _) _ t _) = res
    print $ t * (min p1s p2s)

    -- Part 2
    -- The larger of the two game counts
    print $ evalState (part2 ((7, 0), (5, 0))) M.empty 



part2 :: PState2 -> State (M.Map PState2 (Int, Int)) (Int, Int)
part2 game@((p1p, p1s), (p2p, p2s))
  | p1s >= 21 = return (1, 0)
  | p2s >= 21 = return (0, 1)
  | otherwise = do
        seen <- get
        case seen M.!? game of
            Just res -> return res
            Nothing -> do
                let moves = [ ((p2p, p2s), (p1p', p1p' + p1s)) | d <- [sum [a,b,c] | a<-[1..3], b<-[1..3] ,c<-[1..3]] , let p1p' = move d p1p ]
                futures <- mapM part2 moves
                let res = swap $ foldl sumTuple (0, 0) futures
                modify (M.insert game res)
                return res


sumTuple (a,b) (c,d) = (a+c,b+d)
