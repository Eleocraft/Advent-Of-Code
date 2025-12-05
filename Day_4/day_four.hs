import System.IO
import Debug.Trace (trace)
import Data.List.Split (splitOn, chunksOf)
import Data.Char (digitToInt)
import Data.Array.Unboxed

main = do
        contents <- readFile "input.txt"
        print . calculateFreeRollsRecursive . parseMap $ contents

parseMap :: String -> UArray (Int, Int) Bool
parseMap str = array ((0, 0), (w-1, h-1)) [
            ((x, y), v == '@') |
            (y, line) <- zip [0..] (lines str),
            (x, v) <- zip [0..] line
        ]
        where
            w = case lines str of
                [] -> 0
                vs -> length vs
            h = length . lines $ str

calculateFreeRollsRecursive :: UArray (Int, Int) Bool -> Int
calculateFreeRollsRecursive arr =
        let freeRolls = calculateFreeRolls arr
        in freeRolls + if freeRolls > 0 then calculateFreeRollsRecursive . removeFreeRolls $ arr
           else 0

calculateFreeRolls :: UArray (Int, Int) Bool -> Int
calculateFreeRolls arr = foldr (\v cnt -> cnt + fromEnum v) 0 [
            uncurry isFreeRoll i arr |
            i <- range (bounds arr)
        ]

removeFreeRolls :: UArray (Int, Int) Bool -> UArray (Int, Int) Bool
removeFreeRolls arr = array (bounds arr) [
            (let x = fst i
                 y = snd i
             in (x, y), not (uncurry isFreeRoll i arr) && arr ! i) |
            i <- range (bounds arr)
        ]

isFreeRoll :: Int -> Int -> UArray (Int, Int) Bool -> Bool
isFreeRoll x y arr =
        let
            bound = intersectBounds ((x-1, y-1), (x+1, y+1)) (bounds arr)
        in arr ! (x,y) && 5 > foldr (\v cnt -> cnt + fromEnum v) 0 [
            arr ! i |
            i <- range bound
        ]

        where intersectBounds ((a0,b0),(a1,b1)) ((c0,d0),(c1,d1)) = ((max a0 c0, max b0 d0), (min a1 c1, min b1 d1))
