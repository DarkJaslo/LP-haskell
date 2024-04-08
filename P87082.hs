-- body mass index

imc :: Double -> Double -> Double
imc m h = m / (h*h)

howfat :: Double -> Double -> String
howfat m h
    | x < 18 = "underweight"
    | x >= 18 && x < 25 = "normal weight"
    | x >= 25 && x < 30 = "overweight"
    | x >= 30 && x < 40 = "obese"
    | x >= 40 = "severely obese"
    where
        x = imc m h

main :: IO()
main = do
    line <- getLine

    if(head line) == '*' then 
        return ()
    else do
        let w = words line
        let name =  w !! 0
        let weight = read (w !! 1)
        let height = read (w !! 2)
        putStr (name ++ ": " ++ (howfat weight height) ++ "\n")
        main
