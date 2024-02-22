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

{- Accumulation version
average :: [Int] -> Float
average l = uncurry g $ average' l
    where
        g :: Int -> Int -> Float
        g x y = (fromIntegral x:: Float) / 
                (fromIntegral y:: Float)
        average' :: [Int] -> (Int,Int)
        average' [x] = (x,1)
        average (x:xs) = f x $ average' xs
            where
                f:: Int -> (Int,Int) -> (Int,Int)
                f x t (x + fst t, 1 + snd t)
-}

buildPalindrome' :: [Int] -> [Int]
buildPalindrome' [x] = [x]
buildPalindrome' (x:xs) = (buildPalindrome' xs) ++ [x]

buildPalindrome :: [Int] -> [Int]
buildPalindrome [] = []
buildPalindrome (x:xs) = (buildPalindrome' (x:xs)) ++ (x:xs)

--auxiliary for remove
isIn :: [Int] -> Int -> Bool
isIn [] x = False
isIn [x] y = x==y
isIn (x:xs) y = (isIn [x] y) || (isIn (xs) y)

remove :: [Int] -> [Int] -> [Int]
remove [] [] = []
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

odds :: [Int] -> [Int]
odds [] = []
odds (x:a) = list
    where
        list
            | mod x 2 == 1 = [x] ++ odds a
            | otherwise = odds a

evens :: [Int] -> [Int]
evens [] = []
evens (x:a) = list
    where
        list
            | mod x 2 == 0 = [x] ++ evens a
            | otherwise = evens a

oddsNevens :: [Int] -> ([Int],[Int])
oddsNevens [] = ([],[])
oddsNevens (x:a) = (odds (x:a), evens (x:a))

isPrime :: Int -> Bool
isPrime 0 = False
isPrime 1 = False
isPrime n = isPrime' 2
    where
        isPrime' d
            | mod n d == 0 && d < n = False
            | mod n d >= 1 && d < n = isPrime' (d+1)
            | n < 0 = False
            | otherwise = True

primeDivisors :: Int -> [Int]
primeDivisors x = primeDivisors' 2
    where
        primeDivisors' a
            | a > x = []
            | (isPrime a) && (mod x a == 0) = [a] ++ (primeDivisors' (a+1))
            | otherwise = primeDivisors' (a+1)

