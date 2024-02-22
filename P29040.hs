insert :: [Int] -> Int -> [Int]
insert [] x = [x]
insert (a:r) x
    | x <= a = [x] ++ (a:r)
    | otherwise = [a] ++ insert r x

isort' :: [Int] -> [Int] -> [Int]
isort' [] [] = []
isort' (a:r) [] = (a:r)
isort' [] (b:s) = isort' (insert [] b) s
isort' [a] (b:s) = isort' (insert [a] b) s
isort' (a:r) (b:s) = isort' (insert (a:r) b) s

isort :: [Int] -> [Int]
isort [] = []
isort (a:r) = isort' [] (a:r)

remove :: [Int] -> Int -> [Int]
remove [] x = []
remove (a:r) x
    | a == x = r
    | otherwise = [a] ++ remove r x

minNum :: [Int] -> Int -> Int
minNum [] x = x
minNum (a:r) x
    | a < x = minNum r a
    | otherwise = minNum r x

ssort :: [Int] -> [Int]
ssort [] = []
ssort (a:r) = [(minNum (a:r) a)] ++ (ssort (remove (a:r) (minNum (a:r) a)))

merge :: [Int] -> [Int] -> [Int]
merge [] [] = []
merge (a:r) [] = (a:r)
merge [] (a:r) = (a:r)
merge (a:r) (b:s)
    | a < b = [a] ++ merge r (b:s)
    | otherwise = [b] ++ merge (a:r) s

--msort :: [Int] -> [Int]

--qsort :: [Int] -> [Int]

--genQsort :: Ord a => [a] -> [a]