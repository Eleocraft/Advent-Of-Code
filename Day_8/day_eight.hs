import System.IO
import Debug.Trace (trace, traceShowId)
import Data.List.Split (splitOn, chunksOf, splitWhen)
import Data.List (elemIndex, transpose, sortBy)
import Data.Tuple.Extra (thd3)
import Data.Ord (comparing, Down (Down))
import Data.HashSet (HashSet, insert, member, empty, union)

main = do
        contents <- readFile "input.txt"
        let positions = map parsePos
              . lines $ contents
        let sets = map (`insert` empty) [0..(length positions - 1)]
        print . findConnection sets
              . sortBy (comparing thd4)
              . distances 0 $ positions

thd4 (x, y, z, w) = z

parsePos :: String -> (Int, Int, Int)
parsePos = toCoord . map read . splitOn ","
        where toCoord [x, y, z] = (x, y, z)

distances :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int, Int)]
distances _ [] = []
distances ix (x:xs) = [(ix, iy, sqrdistance x y, resultKey x y) | (iy, y) <- zip [(ix+1)..] xs] ++ distances (ix + 1) xs
        where sqrdistance (x1, y1, z1) (x2, y2, z2) = (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2
              resultKey (x1, y1, z1) (x2, y2, z2) = x1 * x2

findConnection :: [HashSet Int] -> [(Int, Int, Int, Int)] -> Int
findConnection sets ((ix, iy, dist, resultKey):xs) = if break then resultKey else findConnection insertFind xs
        where break = length insertFind == 1
              insertFind = insertFind' (insert ix (insert iy empty)) sets
              insertFind' newSet [] = [newSet]
              insertFind' newSet (set:xs) = if member ix set || member iy set
                                            then insertFind' (newSet `union` set) xs
                                            else set : insertFind' newSet xs


-- First exercise
-- main = do
--         contents <- readFile "input.txt"
--         print 
--           . product
--           . take 3
--           . sortBy (comparing Down)
--           . map length
--           . makeConnections
--           . take 1000
--           . sortBy (comparing thd3)
--           . distances 0
--           . map parsePos
--           . lines $ contents
-- 
-- parsePos :: String -> (Int, Int, Int)
-- parsePos = toCoord . map read . splitOn ","
--         where toCoord [x, y, z] = (x, y, z)
-- 
-- distances :: Int -> [(Int, Int, Int)] -> [(Int, Int, Int)]
-- distances _ [] = []
-- distances ix (x:xs) = [(ix, iy, sqrdistance x y) | (iy, y) <- zip [(ix+1)..] xs] ++ distances (ix + 1) xs
--         where sqrdistance (x1, y1, z1) (x2, y2, z2) = (x1 - x2)^2 + (y1 - y2)^2 + (z1 - z2)^2
-- 
-- makeConnections :: [(Int, Int, Int)] -> [HashSet Int]
-- makeConnections [] = []
-- makeConnections ((ix, iy, dist):xs) = insertFind (ix, iy) $ makeConnections xs
--         where insertFind (ix, iy) = insertFind' (ix, iy) (insert ix (insert iy empty))
--               insertFind' _ newSet [] = [newSet]
--               insertFind' (ix, iy) newSet (set:xs) = if member ix set || member iy set
--                                                      then insertFind' (ix, iy) (newSet `union` set) xs
--                                                      else set : insertFind' (ix, iy) newSet xs
