module Lib
    ( advent
    ) where


import Control.Arrow
import Data.Char
import Data.List
import Data.List.Split
import Numeric

data OpType = Sum|Prod|Min|Max|Lit|Gt|Lt|Eq deriving (Show, Eq)
data Packet
  = Literal { version :: Int, 
              value :: Int }
  | Operator { version :: Int, 
               op::OpType, 
               packets :: [Packet] } deriving Show

fileName = "input.txt"
--fileName = "test.txt"

advent :: IO ()
advent = do
    -- Basic parse
    input <- fmap (lines >>> head >>> map dtoi >>> map hexit >>> flatten) $ readFile fileName
    let (packets, excess) = getPacket ([],input)
    let versionSum = sumVersion packets

    -- Part 1
    print $ sumVersion packets

    -- Part 2
    print $ eval $ head packets

eval :: Packet -> Int
eval (Operator _ Sum p) = sum $ map eval p
eval (Operator _ Prod p) = product $ map eval p
eval (Operator _ Min p) = minimum $ map eval p
eval (Operator _ Max p) = maximum $ map eval p
eval (Literal _ val) = val
eval (Operator _ Gt p) = if (eval $ p!!0) > (eval $ p!!1) then 1 else 0
eval (Operator _ Lt p) = if (eval $ p!!0) < (eval $ p!!1) then 1 else 0
eval (Operator _ Eq p) = if (eval $ p!!0) == (eval $ p!!1) then 1 else 0


sumVersion :: [Packet] -> Int
sumVersion [] = 0
sumVersion ((Literal v _):xs) = v + (sumVersion xs)
sumVersion ((Operator v _ p):xs) = v + (sumVersion xs) + (sumVersion p)

getPacket :: ([Packet],String) -> ([Packet], String)
getPacket (ip,s)
    | typeID == Lit = (ip++[Literal version lit], _lit)
    | lengthID == 1 = (ip++[Operator version typeID packets], _packets)
    | lengthID == 0 = (ip++[Operator version typeID bitpackets], _bitpackets)
      where
        (version, _version) = (binToDec $ take 3 s, drop 3 s)
        (typeID, _typeID) = (binToOp $ take 3 _version, drop 3 _version)
        (lit,_lit) = getLit _typeID
        (lengthID,_lengthID) = (binToDec $ take 1 _typeID, drop 1 _typeID)
        (packetBits,_packetBits) = (binToDec $ take 15 _lengthID, drop 15 _lengthID)
        (packetCount,_packetCount) = (binToDec $ take 11 _lengthID, drop 11 _lengthID)
        (packets, _packets) = last $ take (packetCount+1) $ iterate getPacket ([],_packetCount)
        (bitpackets, _bitpackets) = until (\(p,rs) -> (length _packetBits) - (length rs) == packetBits) getPacket ([],_packetBits)


getLit :: String -> (Int, String)
getLit s = (hexIntToDec h, drop (5 * (length h)) s)
    where
        getLitH ('1':r) = (binToDec $ take 4 r):(getLitH (drop 4 r))
        getLitH ('0':r) = [binToDec $ take 4 r]
        h = getLitH s


flatten :: [[a]] -> [a]         
flatten xs = (\z n -> foldr (\x y -> foldr z y x) n xs) (:) []

binToDec :: String -> Int
binToDec = foldl (\acc x -> acc * 2 + digitToInt x) 0

binToOp :: String -> OpType
binToOp "000" = Sum
binToOp "001" = Prod
binToOp "010" = Min
binToOp "011" = Max
binToOp "100" = Lit
binToOp "101" = Gt
binToOp "110" = Lt
binToOp "111" = Eq

hexIntToDec :: [Int] -> Int
hexIntToDec = foldl (\acc x -> acc * 16 + x) 0

hexit :: Int -> String
hexit 0 = "0000"
hexit 1 = "0001"
hexit 2 = "0010"
hexit 3 = "0011"
hexit 4 = "0100"
hexit 5 = "0101"
hexit 6 = "0110"
hexit 7 = "0111"
hexit 8 = "1000"
hexit 9 = "1001"
hexit 10 = "1010"
hexit 11 = "1011"
hexit 12 = "1100"
hexit 13 = "1101"
hexit 14 = "1110"
hexit 15 = "1111"

dtoi :: Char -> Int
dtoi '0' = 0
dtoi '1' = 1
dtoi '2' = 2
dtoi '3' = 3
dtoi '4' = 4
dtoi '5' = 5
dtoi '6' = 6
dtoi '7' = 7
dtoi '8' = 8
dtoi '9' = 9
dtoi 'A' = 10
dtoi 'B' = 11
dtoi 'C' = 12
dtoi 'D' = 13
dtoi 'E' = 14
dtoi 'F' = 15
