{-# LANGUAGE ParallelListComp #-}

rInt :: String -> Int
rInt = read

findNew :: [Int] -> Bool -> Int
findNew readings fill_first = sum (zipWith (*) coeffs adjustedReadings)
    where 
        maxDepth = length readings-1
        coeffs = [coeff*(1-(2*(i`mod`2))) | coeff <- drop 1 (pascal maxDepth) | i<-[0..]]
        adjustedReadings = if fill_first then readings else reverse readings

pascal :: Int -> [Int]
pascal 0 = [1]
pascal 1 = [1, 1]
pascal n = 1 : pascalStep p ++ [1]
    where 
        p = pascal (n-1)

pascalStep :: [Int] -> [Int]
pascalStep [] = []
pascalStep [_] = []
pascalStep (x:y:xs) = x + y : pascalStep (y : xs)



main :: IO ()
main = do
    contents <- readFile "day9.txt"
    let readings = [map rInt (words x) | x <- lines contents]
    print (sum (map (`findNew` False) readings))
    print (sum (map (`findNew` True) readings))