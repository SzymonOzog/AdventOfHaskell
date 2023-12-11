import Data.List
expandGalaxy:: [String] -> [String]
expandGalaxy [] = []
expandGalaxy (x:xs)
    | isRowEmpty = x : x : expandGalaxy xs
    | otherwise = x : expandGalaxy xs
    where
        isRowEmpty = all (=='.') x

distance :: [Int] -> [Int] -> Int -> (Int,Int) -> (Int,Int) -> Int
distance emptyRows emptyColumns emptyCost (r1,c1) (r2, c2) = abs (r1-r2) + abs (c1-c2) + (numEmpty*emptyCost) - numEmpty
    where
        numEmptyColumns = length (filter (\x -> (x > min c1 c2) && (x < max c1 c2)) emptyColumns)
        numEmptyRows = length (filter (\x -> (x > min r1 r2)  && (x < max r1 r2)) emptyRows)
        numEmpty = numEmptyColumns + numEmptyRows

allPairDistance :: [(Int,Int)] -> [Int] -> [Int] -> Int -> Int
allPairDistance [] _ _ _ = 0
allPairDistance (x:xs) emptyRows emptyColumns emptyCost = sum (map (distance emptyRows emptyColumns emptyCost x) xs) + allPairDistance xs emptyRows emptyColumns emptyCost

main :: IO()
main = do
    contents <- readFile "day11.txt"
    let galaxyMap = lines contents

    let emptyRows = findIndices (all (=='.')) galaxyMap
    let emptyColumns = findIndices (all (=='.')) (transpose galaxyMap)

    let totalR = length galaxyMap
    let totalC = length (head galaxyMap)
    let allGalaxies = [(x `div` totalC, x `mod` totalC) | x <- elemIndices '#' (concat galaxyMap)]
    print (allPairDistance allGalaxies emptyRows emptyColumns 2)
    print (allPairDistance allGalaxies emptyRows emptyColumns 1000000)