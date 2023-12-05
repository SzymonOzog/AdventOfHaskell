import Data.Char

getMap :: [String] -> [(Int, Int, Int)]
getMap mapLines = map (\x -> (rInt (x!!0), rInt (x!!1),rInt (x!!2) )) w
    where
        w = map words mapLines

splitList :: [String] -> (String->Bool)-> [[String]]
splitList [] _ = []
splitList l pred = takeWhile pred l : splitList (drop 1 (dropWhile pred l)) pred

getLocation :: Int -> [[(Int, Int, Int)]] -> Int
getLocation = foldl getIdentifier

getIdentifier :: Int -> [(Int, Int, Int)] -> Int
getIdentifier key [] = key
getIdentifier key ((a,b,c):xs)
    | keyInRange = mappedKey
    | otherwise = getIdentifier key xs
    where
        keyInRange = key>=b && key < b+c
        mappedKey = a + key - b

rInt :: String -> Int
rInt = read
main :: IO ()
main = do
    contents <- readFile "day5.txt"
    let contentLines = lines contents
    let seeds = map rInt (drop 1 (words (head contentLines)))
    let all_maps = filter (/="") (drop 3 contentLines)
    let filteredMaps = map getMap (splitList all_maps (isDigit . head))
    print (minimum ( map (`getLocation` filteredMaps) seeds))