import Data.List
import Data.Maybe

symbols :: String
symbols = "*%#@+-=/&$"

isNum :: Char -> Bool
isNum a = a>='0' && a<='9'

distance :: (Ord a, Num a) => (a, a) -> (a, a) -> a
distance (r1,c1) (r2,c2) = max (abs (r1-r2)) (abs (c1-c2))

toRowColumn :: Integral b => b -> b -> (b, b)
toRowColumn i columnLength = (i `div` columnLength, i `mod` columnLength)

isAdjacent :: Integral a => a -> a -> a -> Bool
isAdjacent columnLength a b = b `mod` columnLength /= 0 && b-a == 1

parseNumber :: Int -> String -> [Int] -> (Int, (Int,Int), Int)
parseNumber columnLength content indices = (num, toRowColumn (head indices) columnLength, length indices)
    where
        num = read [content !! i | i <- indices]

extractNumbers :: Int -> [Int] -> [[Int]]
extractNumbers _ [] = []
extractNumbers _ [x] = [[x]]
extractNumbers columns (x:(x2:xs))
    | isAdjacent columns x x2 = ([x, x2] ++ takeWhile (isAdjacent columns x2) xs) : extractNumbers columns (dropWhile (isAdjacent columns x2) xs)
    | otherwise = [x] : extractNumbers columns (x2:xs)

parseSymbols :: String -> Int -> [(Int,Int)]
parseSymbols content columnLength = map (`toRowColumn` columnLength) (findIndices isSymbol content)

isSymbol :: Char -> Bool
isSymbol c = c `elem` symbols

isNextToSymbol :: (Int, (Int, Int), Int) -> (Int,Int)  -> Bool
isNextToSymbol (_, (_,_), 0) (_,_)= False
isNextToSymbol (n, (row, column), length) (sr,sc) = distance (row,column) (sr,sc) == 1 || isNextToSymbol (n, (row, column+1), length-1) (sr,sc)

filterNumbersNextToSymbols :: [(Int, (Int, Int), Int)] -> [(Int,Int)] -> [(Int, (Int, Int), Int)]
filterNumbersNextToSymbols _ [] = []
filterNumbersNextToSymbols numbers symbols = filter (\number -> any ( isNextToSymbol number) symbols) numbers -- ++ filterNumbersNextToSymbols numbers xs

main :: IO ()
main = do
    fileContents <- readFile "../day3.txt"
    let columns = fromJust (elemIndex '\n' fileContents)
    let contents = filter ('\n'/=) fileContents
    let test = findIndices isNum contents
    let grouped = extractNumbers columns test
    let numbers = map (parseNumber columns contents) grouped
    let symbolLocs = parseSymbols contents columns
    print (sum [num | (num,_,_) <- filterNumbersNextToSymbols numbers symbolLocs])