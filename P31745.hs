flatten:: [[Int]] -> [Int]
flatten l = foldr (++) [] l

myLength :: String -> Int
myLength l = foldl (+) 0 (map (const 1) l)

myReverse :: [Int] -> [Int]
myReverse l = foldl (flip (++)) [] (map (\x -> [x]) l)

countIn :: [[Int]] -> Int -> [Int]
countIn l y = map (length . filter (\z -> z==y)) l

firstWord :: String -> String
firstWord l = takeWhile (\x -> x/=' ')  (dropWhile (\x -> x==' ') l)