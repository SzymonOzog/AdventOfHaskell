import Data.List
import Debug.Trace
wordsWhen :: (Char -> Bool) -> String -> [String]
wordsWhen p s =  case dropWhile p s of
                      "" -> []
                      s' -> w : wordsWhen p s''
                            where (w, s'') = break p s'

parseRecord :: String -> (String, [Int])
parseRecord record = (spring, groups) 
    where
        spring = head (words record)
        groups = map rInt ((wordsWhen (==',') ((words record)!!1)))

rInt :: String -> Int
rInt = read

isPossible :: (String, [Int]) -> Bool
isPossible record =  conditions == (snd record)
    where
        grouped = groupBy (\x y -> (x=='#') && (y=='#')) (fst record)
        conditions = map length (filter (/=['.']) grouped)

countPossible :: (String, [Int]) -> Int
countPossible record = sum ([if isPossible (x,snd record) then 1 else 0 | x <- allSpringPermutations (fst record)])

allSpringPermutations :: String -> [String]
allSpringPermutations record 
    | '?' `elem` record = allSpringPermutations (replaceFirst record '?' '.') ++ allSpringPermutations (replaceFirst record '?' '#')
    | otherwise = [record]

replaceFirst :: String -> Char -> Char -> String
replaceFirst s c1 c2 = (takeWhile (/=c1) s) ++ [c2] ++ (drop 1 (dropWhile (/=c1) s))

main :: IO()
main = do
    contents <- readFile "day12.txt"
    let springRecors = map parseRecord (lines contents) 
    print(sum (map countPossible springRecors))