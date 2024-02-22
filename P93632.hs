eql :: [Int] -> [Int] -> Bool
eql [] [] = True
eql (a:s) [] = False
eql [] (a:s) = False
eql (a:r) (b:s) = length (a:r) == length (b:s) && all (\x -> x == 0) (zipWith (-) (a:r) (b:s))

prod :: [Int] -> Int
prod [] = 1
prod (a:r) = foldl (*) 1 (a:r)

prodOfEvens :: [Int] -> Int
prodOfEvens [] = 1
prodOfEvens (a:r) = prod $ filter even (a:r)

powersOf2 :: [Int]
powersOf2 = iterate (\x -> x*2) 1

scalarProduct :: [Float] -> [Float] -> Float
scalarProduct (a:r) (b:s) = foldl (+) 0.0 (zipWith (*) (a:r) (b:s)) 

--extra
allEqual :: [Int] -> Bool
allEqual [] = True
allEqual (x:r) = all (\y -> y==x) (x:r)