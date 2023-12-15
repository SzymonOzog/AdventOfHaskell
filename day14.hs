import Data.List
import Data.Maybe
import Debug.Trace (traceShow)

toRowColumn :: Int -> Int -> (Int,Int)
toRowColumn i columnLength = (i `div` columnLength, i `mod` columnLength)

findRow :: [Int] -> Int -> Int
findRow takenRows current
    | (current-1) `elem` takenRows || (current==0) = current
    | otherwise = findRow takenRows (current-1)


updatePosition :: [(Int,Int)] -> (Int,Int) -> (Int,Int)
updatePosition takenPositions (r,c) = (minRow, c)
    where
        inColumn = filter ((==c) . snd) takenPositions
        minRow = findRow (map fst inColumn) r

updateAll :: [(Int,Int)] -> [(Int,Int)] -> [(Int,Int)]
updateAll [] _ = []
updateAll (x:xs) squarePos = updatePosition (updated ++ squarePos) x : updated
    where
         updated = updateAll xs squarePos

main :: IO ()
main = do
    contents <- readFile "day14.txt"
    let columns = fromMaybe 0 (elemIndex '\n' contents)
    let rows = 1 + length (elemIndices '\n' contents)
    let filtered =  filter (/='\n') contents
    let roundRocks = [toRowColumn i columns | i <- elemIndices 'O' filtered]
    let squareRocks = [toRowColumn i columns | i <- elemIndices '#' filtered]
    let takenPositions = roundRocks ++ squareRocks
    let updated = updateAll (reverse roundRocks) squareRocks
    print (sum (map (\(r,c) -> rows-r ) updated))
