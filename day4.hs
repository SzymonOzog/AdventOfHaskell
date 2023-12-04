import Data.List
main :: IO ()
main = do
    contents <- readFile "day4.txt"
    let contetnLines = lines contents
    let filtered = map (drop 1 . dropWhile (/= ':')) contetnLines
    let winningToYour = [ (words w, words(drop 1 y))| x <- filtered, let (w,y) = span (/= '|') x ]
    let won = [length (y `intersect` w) | (w,y) <- winningToYour]
    let points = [2 ^ (x-1) | x <- filter (>0) won]  
    print (sum points)