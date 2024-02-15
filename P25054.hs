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

--buildPalindrome :: [Int] -> [Int]

--remove :: [Int] -> [Int] -> [Int]

--flatten :: [[Int]] -> [Int]

--oddsNevens :: [Int] -> ([Int],[Int])

--primeDivisors :: Int -> [Int]

