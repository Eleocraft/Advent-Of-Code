import System.IO
import Debug.Trace (trace, traceShowId)
import Data.List.Split (splitOn, chunksOf, splitWhen)
import Data.List (elemIndex, transpose, sortBy, find, partition)
import Data.Ord (comparing, Down (Down))
import Data.HashSet (HashSet, insert, member, empty, union)
import Data.Maybe (fromJust)

type Rect = ((Int, Int), (Int, Int))
type Line = (Int, Int, Int)
main = do
        contents <- readFile "input.txt"
        let redTiles = map parsePos
              . lines $ contents
        let (horizontals, verticals) = partition (\((x1, y1), (x2, y2)) -> y1 == y2) . windows2 $ last redTiles : redTiles
        let lines = (map (\((x1, y1), (x2, y2)) -> (min x1 x2, max x1 x2, y1)) horizontals,
                     map (\((x1, y1), (x2, y2)) -> (min y1 y2, max y1 y2, x1)) verticals)
        let bounds = (maximum . map fst $ redTiles, maximum . map snd $ redTiles)
        print . snd
              . fromJust
              . find (\(vertices, area) -> not $ intersectingArea lines vertices)
              . sortBy (comparing (Down . snd))
              . areas 0 $ redTiles

windows2 :: [a] -> [(a, a)]
windows2 (x:xs) = zip (x:xs) xs

parsePos :: String -> (Int, Int)
parsePos = toCoord . map read . splitOn ","

toCoord :: [a] -> (a, a)
toCoord [x, y] = (x, y)

areas :: Int -> [(Int, Int)] -> [(Rect, Int)]
areas _ [] = []
areas ix (x:xs) = [(sortC x y, area x y) | (iy, y) <- zip [(ix+1)..] xs] ++ areas (ix + 1) xs
        where area (x1, y1) (x2, y2) = (1 + abs (x2 - x1)) * (1 + abs (y2 - y1))
              sortC (x1, y1) (x2, y2) = ((min x1 x2, min y1 y2), (max x1 x2, max y1 y2))

intersectingArea :: ([Line], [Line]) -> Rect -> Bool
intersectingArea (horizontals, verticals) ((x1, y1), (x2, y2)) = rectLinesIntersectH ((x1, y1), (x2, y2)) horizontals
                                                              || rectLinesIntersectH ((y1, x1), (y2, x2)) verticals

rectLinesIntersectH :: Rect -> [Line] -> Bool
rectLinesIntersectH _ [] = False
rectLinesIntersectH rect (line:lines) = rectLineIntersect rect line || rectLinesIntersectH rect lines
        where rectLineIntersect ((x1, y1), (x2, y2)) (lcol1, lcol2, lperp) = lcol1 < x2 && lcol2 > x1 && lperp < y2 && lperp > y1;

-- first try at second exercise, keeping for reference
-- isGreen :: ([Line], [Line]) -> (Int, Int) -> Int -> Int -> Bool
-- isGreen (horizontals, verticals) (sizeX, sizeY) x y = odd $ case minPath of
--                                                       0 -> checkLines verticals (<=) y x
--                                                       1 -> checkLines horizontals (<=) x y
--                                                       2 -> checkLines verticals (>=) y x
--                                                       3 -> checkLines horizontals (>=) x y
--         where paths = [x, y, sizeX - x, sizeY - y]
--               minPath = fromJust $ elemIndex (minimum  paths) paths
-- 
-- checkLines :: [Line] -> (Int -> Int -> Bool) -> Int -> Int -> Int
-- checkLines [] _ _ _ = 0
-- checkLines (line:lines) perpFn col perp = fromEnum (intersect line perpFn col perp) + checkLines lines perpFn col perp
-- 
-- intersect :: Line -> (Int -> Int -> Bool) -> Int -> Int -> Bool
-- intersect (lcol1, lcol2, lperp) perpFn col perp = lperp `perpFn` perp && (col <= lcol2 && col >= lcol1)

-- # first exercise
-- main = do
--         contents <- readFile "test.txt"
--         print . maximum
--               . map thd3
--               . areas 0
--               . map parsePos
--               . lines $ contents
-- 
-- parsePos :: String -> (Int, Int)
-- parsePos = toCoord . map read . splitOn ","
--         where toCoord [x, y] = (x, y)
-- 
-- areas :: Int -> [(Int, Int)] -> [(Int, Int, Int)]
-- areas _ [] = []
-- areas ix (x:xs) = [(ix, iy, area x y) | (iy, y) <- zip [(ix+1)..] xs] ++ areas (ix + 1) xs
--         where area (x1, y1) (x2, y2) = (1 + abs (x2 - x1)) * (1 + abs (y2 - y1))
