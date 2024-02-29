--Usage of comprehension lists

myMap :: (a -> b) -> [a] -> [b]
myMap f a = [f x | x <- a ]

myFilter :: (a -> Bool) -> [a] -> [a]
myFilter f a = [x | x <- a, f x ]

myZipWith :: (a -> b -> c) -> [a] -> [b] -> [c]
myZipWith f a b = [f x y | (x,y) <- zip a b]

thingify :: [Int] -> [Int] -> [(Int,Int)]
thingify a b = [(x,y) | x <- a, y <- b, x `mod` y == 0]

factors :: Int -> [Int]
factors a = [x | x <- [1..a], a `mod` x == 0]