import System.IO

main = do
        contents <- readFile "test.txt"
        print . getNumZeros 50 . map readInfo . lines $ contents

readInfo :: String -> (Char, Int)
readInfo (h:t) = (h, read t)

getNumZeros :: Int -> [(Char, Int)] -> Int
getNumZeros _ [] = 0
getNumZeros x ((dir, n):rest) = ifZero newPos + getNumZeros newPos rest
        where newPos = case dir of
                            'L' -> mod (x - n) 100
                            'R' -> mod (x + n) 100
              ifZero y = if y == 0 then 1 else 0
