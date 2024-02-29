-- Infinite lists(1)

ones :: [Integer]
ones = iterate id 1

nats :: [Integer]
nats = iterate (+1) 0

ints :: [Integer]
ints = 0 : ints' 1
    where
        ints' a = a : -a : ints' (a+1)

triangulars :: [Integer]
triangulars = triangulars' 0 1
    where
        triangulars' a b = a : triangulars' (a+b) (b+1)

factorials :: [Integer]
factorials = 1: factorials' 1 1
    where  
        factorials' a b = a*b : factorials' (a*b) (b+1)

fibs :: [Integer]
fibs = 0 : 1: fibs' 0 1
    where
        fibs' a b = a+b : fibs' b (a+b)

primes :: [Integer]
primes = primes' [2..]
    where
        primes' (a:r) = a : primes' [x | x <- r, x `mod` a /= 0]

hammings :: [Integer]
--hammings = 1: filter (\x -> x `mod` 2 == 0 || x `mod` 3 == 0 || x `mod` 5 == 0) (iterate (+1) 2)

--lookNsay :: [Integer]
 
--tartaglia :: [[Integer]]