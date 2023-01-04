-- Haskell assignment to add all the perfect numbers up to a trillion

-- returns true if p is an exponent which gives a Mersenne prime, otherwise false
isMersenne :: Int -> Bool

isMersenne p = 0 == getTotalS p 4 (2^p - 1)


-- returns the value of s once s has been changed with the formula p - 2 times
-- reference for formula: https://en.wikipedia.org/wiki/Lucas%E2%80%93Lehmer_primality_test
getTotalS :: Int -> Int -> Int -> Int

getTotalS 2 4 3 = 0
getTotalS 2 s m = s
getTotalS p s m = getTotalS (p - 1) (mod ((s * s) - 2) m) m


-- returns the perfect number associated with the exponent which gives a Mersenne prime (x)
-- reference for formula: https://www.mathnasium.com/www-mathnasium-com-fremont-news-the-largest-perfect-number
perfectNumber :: Int -> Int

perfectNumber x = (2^(x - 1)) * ((2^x) - 1)


-- returns the sum of all perfect numbers from 1 to 1 trillion
sumPerfects :: Int

sumPerfects = foldl (+) 0 (takeWhile (<1000000000000) (map perfectNumber (filter isMersenne ([2] ++ (filter odd [3..])))))
