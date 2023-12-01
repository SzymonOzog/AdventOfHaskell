import System.IO
import Data.Text(pack, unpack, replace, isPrefixOf, isSuffixOf)

strToNum=[("one", "1"),
        ("two", "2"),
        ("three", "3"),
        ("four", "4"),
        ("five", "5"),
        ("six", "6"),
        ("seven", "7"),
        ("eight", "8"),
        ("nine", "9")]

lsum :: Num a => [a] -> a
lsum [] = 0
lsum (x:xs) = x + sum xs

parseNums :: [[Char]] -> [Integer]
parseNums lines = map parseNum lines

parseNum :: [Char] -> Integer
parseNum line = createCalibationValue (getFirstNum (replaceFirst line strToNum)) (getFirstNum(reverse (replaceLast line strToNum)))

getFirstNum :: [Char] -> Integer
getFirstNum line = read(take 1(filter isNum line))

isNum a = a>='0' && a<='9'

createCalibationValue first second = 10*first + second 

replaceFirst :: String -> [(String, String)] -> String
replaceFirst "" [] = ""
replaceFirst str [] = take 1 str ++ replaceFirst(drop 1 str) strToNum
replaceFirst str ((old, new):repls) = 
    if pack old `isPrefixOf` pack str
    then unpack (pack new) ++ replaceFirst (drop (length old) str) strToNum
    else replaceFirst str repls

replaceLast :: String -> [(String, String)] -> String
replaceLast "" [] = ""
replaceLast str [] = replaceLast(reverse(drop 1 (reverse str))) strToNum ++ take 1 (reverse str)
replaceLast str ((old, new):repls) = 
    if pack old `isSuffixOf` pack str
    then replaceLast (reverse(drop (length old) (reverse str))) strToNum ++ unpack (pack new) 
    else replaceLast str repls


main :: IO ()

main = do
    contents <- readFile "data.txt"
    putStrLn(show(lsum(parseNums(lines contents))))