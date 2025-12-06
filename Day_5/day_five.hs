import System.IO
import Debug.Trace (trace, traceShowId)
import Data.List.Split (splitOn, chunksOf)
import Data.List (elemIndex)
import Data.Char (digitToInt)
import Data.Maybe (fromMaybe)

main = do
        contents <- readFile "input.txt"
        let emptyIndex = fromMaybe 0 . elemIndex "" . lines $ contents
        let (ranges, _) = splitAt emptyIndex . lines $ contents
        print . countFresh . traceShowId . combineRanges . map readRange $ ranges
      

readRange :: String -> (Int, Int)
readRange s = 
    let (a, b) = break (== '-') s
    in (read a, read $ drop 1 b)

countFresh :: [(Int, Int)] -> Int
countFresh = foldr (\(a,b) -> (+) (b - a + 1)) 0

combineRanges :: [(Int, Int)] -> [(Int, Int)]
combineRanges [] = []
combineRanges (x:rng) = combineInto x $ combineRanges rng
    where combineInto x [] = [x]
          combineInto x (r:rng) = 
              let combined = combineTwo x r
              in case combined of Just x -> combineInto x rng
                                  Nothing -> r : combineInto x rng
          combineTwo (x1, y1) (x2, y2) = if (y1 >= x2 && x1 <= y2) || (y2 >= x1 && x2 <= y1)
              then Just (min x1 x2, max y1 y2) else Nothing

-- First task
-- main = do
--         contents <- readFile "input.txt"
--         let emptyIndex = fromMaybe 0 . elemIndex "" . lines $ contents
--         let (ranges, e:ids) = splitAt emptyIndex . lines $ contents
--         print $ countFresh (map readRange ranges) (map readNum ids)
--       
-- 
-- readRange :: String -> (Int, Int)
-- readRange s = 
--     let (a, b) = break (== '-') s
--     in (read a, read $ drop 1 b)
-- 
-- 
-- readNum :: String -> Int
-- readNum = read
-- 
-- countFresh :: [(Int, Int)] -> [Int] -> Int
-- countFresh _ [] = 0
-- countFresh rng (i:ir) = fromEnum (inRanges rng i) + countFresh rng ir
--     where inRanges [] i = False
--           inRanges (v:rng) i = inBound v i || inRanges rng i
--           inBound (low, high) i = i <= high && i >= low
