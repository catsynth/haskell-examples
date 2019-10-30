import Prelude hiding (sum)

naturals = [1..]


sum :: Num a => Int -> [a] -> a
sum n series = foldl (+) 0 (take n series)


harmonics = [1/x    | x <- naturals]
inverseSquares = [1/x**2 | x <- naturals]
inverseCubes   = [1/x**3 | x <- naturals]

harmonicSeries n = sum n harmonics
inverseSquaresSeries n = sum n inverseSquares
inverseCubesSeries n = sum n inverseCubes


powerSeries n = [1/n**x  | x <- naturals]

power2 = powerSeries 2
power3 = powerSeries 3


factorial :: (Num a, Eq a) => a -> a
factorial 0 = 1
factorial n = n * (factorial (n - 1))

factorials = [1/(factorial x) | x <- 0:naturals]

factorialSeries n = sum n factorials
