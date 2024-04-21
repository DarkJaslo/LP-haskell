-- Taylor del cosinus

termes_cosinus :: Double -> [Double]
termes_cosinus alfa = map primer (iterate seguent (1, 1, alfa))

seguent :: (Double, Int, Double) -> (Double, Int, Double)
seguent (term, n, alfa) = (term', n+1, alfa)
  where
    term' = ((-1) * term * alfa * alfa) / ((2* (fromIntegral n) - 1) * (2* (fromIntegral n)))

primer :: (a,b,c) -> a
primer (x,y,z) = x

cosinus :: Double -> Double -> Double
cosinus alfa epsilon = foldl (+) 0 (takeWhile (absGreater epsilon) (termes_cosinus alfa))

absGreater :: Double -> Double -> Bool
absGreater epsilon x
  | x < 0     = -x > epsilon
  | otherwise =  x > epsilon
