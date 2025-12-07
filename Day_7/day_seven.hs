import System.IO
import Debug.Trace (trace, traceShowId)
import Data.List.Split (splitOn, chunksOf, splitWhen)
import Data.List (elemIndex, transpose)
--import Data.HashSet (HashSet, empty, insert, fromList, union)
import Data.HashMap.Strict (HashMap, empty, insertWith, fromList, unionWith, foldrWithKey)
import Data.Array.Unboxed (UArray, array, (!))

main = do
        contents <- readFile "input.txt"
        let map = parseMap . lines $ contents
        let startPosX = startPos 0 $ head map
        print . sum . traceLines (fromList [(startPosX,1)]) $ tail map


parseMap :: [String] -> [UArray Int Bool]
parseMap [] = []
parseMap lines = map (\line -> array (0, length line - 1) [
            (
              x, case v of '.' -> False
                           '^' -> True
                           'S' -> True
            ) |
            (x, v) <- zip [0..] line
        ]) lines

startPos :: Int -> UArray Int Bool -> Int
startPos x arr = if arr ! x then x else startPos (x + 1) arr

traceLines :: HashMap Int Int -> [UArray Int Bool] -> HashMap Int Int
traceLines xs [] = xs
traceLines xs (line:rest) = traceLines processedRow rest
        where processedRow = foldrWithKey (\key value set -> if line ! key then insertWith (+) (key + 1) value (insertWith (+) (key - 1) value set) else insertWith (+) key value set) empty xs

-- First problem
-- traceLines :: HashSet Int -> [UArray Int Bool] -> Int
-- traceLines _ [] = 0
-- traceLines xs (line:rest) = numberOfSplits + traceLines processedRow rest
--         where (processedRow, numberOfSplits) = foldr (\y (set, num) -> (if line ! y then insert (y + 1) (insert (y - 1) set) else insert y set, num + fromEnum (line ! y))) (empty, 0) xs
