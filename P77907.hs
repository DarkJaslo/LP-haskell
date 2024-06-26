absValue :: Int -> Int
absValue n
    | n>=0 = n
    | otherwise = -n

power :: Int -> Int -> Int
power x 0 = 1
power x y = x * power x (y-1)

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

slowFib :: Int -> Int
slowFib n
    | n <= 1 = n
    | otherwise = slowFib(n-2) + slowFib(n-1)

quickFib :: Int -> Int
quickFib n = snd (fib n) -- hem d'agafar el segon element de la tupla
    where
        fib 0 = (0,0)
        fib 1 = (0,1)
        fib i = (f1, f1+f2)
            where
                (f2,f1) = fib (i-1)

{- comentari
multi
linia -}