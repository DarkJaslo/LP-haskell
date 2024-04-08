-- body mass index (without * end mark ver., end input with ctrl+D)

imc :: Double -> Double -> Double
imc m h = m / (h*h)

howfat' :: Double -> Double -> String -> String
howfat' m h name
    | name == "*" = ""
    | x < 18 = name ++ ": underweight"
    | x >= 18 && x < 25 = name ++ ": normal weight"
    | x >= 25 && x < 30 = name ++ ": overweight"
    | x >= 30 && x < 40 = name ++ ": obese"
    | x >= 40 = name ++ ": severely obese"
    where
        x = imc m h

howfat :: String -> String
howfat l = howfat' m h name
    where
        w = words l
        m = read(w !! 1)
        h = read(w !! 2)
        name = w !! 0

main :: IO()
main = do
    contents <- getContents
    mapM_ (print . howfat) (lines contents)