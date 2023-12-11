import Data.List
import Debug.Trace (traceShow)
import Data.Char (isDigit)
constrains = [("blue", 14),("green", 13), ("red", 12)]
maxb = tsum constrains
tsum :: [(String, Int)] -> Int
tsum [] = 0
tsum ((_, i):xs) = i + tsum xs

isPossible :: [(String, Int)] -> [(String, Int)] -> Bool
isPossible [] [] = True
isPossible (x:xs) [] = isPossible xs []
isPossible [] (x:xs) = isPossible [] xs
isPossible ((color, outcome) : xs) ((color2, constrain) : xs2)
    | sum1 > maxb = False
    | color == color2 = (outcome <= constrain) && isPossible xs xs2
    | otherwise = isPossible ((color, outcome) : xs) xs2 && isPossible xs ((color2, constrain) : xs2)
    where
        sum1 = tsum  ((color, outcome) : xs)

parseGame :: [String] -> [(String, Int)]
parseGame  (w:(w2:xs))
    | null xs = [(w2, read w)]
    | otherwise = (w2, read w) : parseGame xs

sumPossible :: [String] -> Int
sumPossible games
    | null games = 0
    | isPossibleGame (filter (/=':') right) = read game_num + sumPossible xs
    | otherwise = sumPossible xs
    where
        (game:xs) = games
        (left, b:right) = break (==':') game
        game_num = filter isDigit left

isPossibleGame :: String -> Bool
isPossibleGame game
    | null game = True
    | null right = isPossible (parseGame (words left)) constrains
    | otherwise = isPossible (parseGame (words left)) constrains && isPossibleGame (drop 1 right)
    where
        (left, right) = break (==';') game

wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseGames :: [String] -> [[(String, Int)]]
parseGames (x:xs)
    | null xs = [concatMap (parseGame . words) individualGames]
    | otherwise = concatMap (parseGame . words) individualGames : parseGames xs
    where
        (left, b:right) = break (==':') x
        individualGames = wordsWhen (==';') right

getMinRequired :: [(String,Int)] -> (Int,Int,Int)
getMinRequired game = (minRed, minBlue, minGreen)
    where
        minRed = maximum (map snd (filter (\x -> fst x =="red") game))
        minBlue = maximum (map snd (filter (\x -> fst x =="blue") game))
        minGreen = maximum (map snd (filter (\x -> fst x =="green") game))

main :: IO ()
main = do
    contents <- readFile "day2.txt"
    let games = lines (filter (/=',') contents)
    print (sumPossible games)
    print (sum (map ((\(r,g,b) -> r*g*b) . getMinRequired) (parseGames games)))

