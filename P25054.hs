myLength :: [Int] -> Int
myLength [] = 0
myLength (x:xs) = 1 + (myLength xs)

myMaximum :: [Int] -> Int
myMaximum [x] = x
myMaximum (x:xs) = max x ( myMaximum xs)
--myMaximum (x:xs) = max x $ myMaximum xs  <- treu els parentesis de la dreta

mySum :: [Int] -> Int
mySum [] = 0
mySum (x:xs) = x + (mySum xs)

average :: [Int] -> Float
average (x:xs) = s / l 
    where
        s = fromIntegral (mySum (x:xs))
        l = fromIntegral (myLength (x:xs))

buildPalindrome' :: [Int] -> [Int]
buildPalindrome' [x] = [x]
buildPalindrome' (x:xs) = (buildPalindrome' xs) ++ [x]

buildPalindrome :: [Int] -> [Int]
buildPalindrome (x:xs) = (buildPalindrome' (x:xs)) ++ (x:xs)

--auxiliary for remove
isIn :: [Int] -> Int -> Bool
isIn [] x = False
isIn [x] y = x==y
isIn (x:xs) y = (isIn [x] y) || (isIn (xs) y)

remove :: [Int] -> [Int] -> [Int]
remove [] (y:ys) = []
remove (x:xs) [] = (x:xs)
remove (x:xs) (y:ys) = list
    where
        list 
            |  isIn (y:ys) x = remove xs (y:ys)
            |  otherwise = [x] ++ (remove xs (y:ys))


flatten :: [[Int]] -> [Int]
flatten [] = []
flatten (x:xs) = x ++ (flatten xs) 

--oddsNevens :: [Int] -> ([Int],[Int])

--primeDivisors :: Int -> [Int]

