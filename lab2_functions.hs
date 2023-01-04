-- lab 2

-- q1: area of a triangle

triangleArea :: Float -> Float -> Float -> Float

-- triangleArea a b c = sqrt(s * (s-a) * (s-b) * (s-c)) where s = (a+b+c) / 2

-- q2: sum test

isSum :: Int -> Int -> Int -> Bool

isSum x y z
   | x + y == z = True
   | x + z == y = True
   | y + z == x = True
   | otherwise = False

-- q3: area of a triangle (revisited)

isTriangle :: Float -> Float -> Float -> Bool

isTriangle a b c = a + b > c && a + c > b && b + c > a

triangleArea a b c
   | isTriangle a b c == False = error "Not a triangle!"

triangleArea a b c = sqrt(s * (s-a) * (s-b) * (s-c)) where s = (a+b+c) / 2
