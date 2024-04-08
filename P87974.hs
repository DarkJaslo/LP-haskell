sayHi :: Char -> IO ()
sayHi c
    | c == 'A' || c == 'a'  = putStrLn "Hello!"
    | otherwise = putStrLn "Bye!"

main :: IO ()
main = do
    line <- getLine
    sayHi (head line)