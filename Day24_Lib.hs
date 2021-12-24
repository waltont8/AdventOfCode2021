module Lib
    ( advent
    ) where


import Control.Arrow
import qualified Data.Set as Set
import Data.Char
import Data.List
import Data.List.Split
import qualified Data.Map as M
import Debug.Trace

data Op = Mod | Div | Add | Mul | Eql | Inp deriving (Show, Eq)
data Var = W | X | Y | Z | Imm Int deriving (Show, Eq)
data State = State {w::Int, x::Int, y::Int, z::Int, inputs::[Int]} deriving (Show, Eq)
type Inst = (Op, Var, Var)

fileName = "input.txt"
--fileName = "test.txt"


advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> map parse) $ readFile fileName
    let a = foldl (\s i -> exec s i) (State 0 0 0 0 [0,0,0,0,0,0,0,0,0,0,0,0,0,0]) input

    -- Part 1
    print $ a

    -- Part 2
    --print $ input

parse :: String -> Inst
parse s = parse' (splitOn " " s)
    where
        parse' ("inp":w:[]) = (Inp, readVar w,W)
        parse' ("mod":a:b:[]) = (Mod, readVar a,readVar b)
        parse' ("div":a:b:[]) = (Div, readVar a,readVar b)
        parse' ("add":a:b:[]) = (Add, readVar a,readVar b)
        parse' ("mul":a:b:[]) = (Mul, readVar a,readVar b)
        parse' ("eql":a:b:[]) = (Eql, readVar a,readVar b)

exec :: State -> Inst -> State
exec s (i,a,b)
    | i == Mod = changeState s a ((getState s a) `mod` (getState s b))
    | i == Div = changeState s a ((getState s a) `div` (getState s b))
    | i == Add = changeState s a ((getState s a) + (getState s b))
    | i == Mul = changeState s a ((getState s a) * (getState s b))
    | i == Eql = changeState s a (if (getState s a) == (getState s b) then 1 else 0)
    | i == Inp = doInput s a

doInput (State w x y z (h:xs)) W = (State h x y z xs)

getState :: State -> Var -> Int
getState (State w x y z _) W = w
getState (State w x y z _) X = x
getState (State w x y z _) Y = y
getState (State w x y z _) Z = z
getState (State w x y z _) (Imm i) = i

changeState :: State -> Var -> Int -> State
changeState (State w x y z l) W i = (State i x y z l)
changeState (State w x y z l) X i = (State w i y z l)
changeState (State w x y z l) Y i = (State w x i z l)
changeState (State w x y z l) Z i = (State w x y i l)

-- Other

readVar "w" = W
readVar "x" = X
readVar "y" = Y
readVar "z" = Z
readVar x = (Imm (readInt x))

readInt :: String -> Int
readInt s = if (head s == '-') then negate (read $ tail s) else (read s)
