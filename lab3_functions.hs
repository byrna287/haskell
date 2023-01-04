-- lab 3

-- q1: palindromes

isPalindrome :: Eq a => [a] -> Bool   -- adds an equals constraint on a

isPalindrome [] = False
isPalindrome lst = lst == reverse(lst)

-- q2: shortest list

shortest :: [[a]] -> [a]

shortest [] = []
shortest [x] = x
shortest (x:xs) = if length x < length sh_lst
                  then x
                  else sh_lst
                  where sh_lst = shortest(xs)

-- q3: adding 2 polynomials

type Poly = [Int]

sumPoly :: Poly -> Poly -> Poly

sumPoly [] [] = []
sumPoly [x] [] = [x]
sumPoly [] [y] = [y]
sumPoly (x:xs) (y:ys) = (x + y:sumPoly xs ys)

-- q4: evaluating a polynomial

evalPoly :: Int -> [Int] -> Int
--eval :: Int -> [Int] -> Int

--evalPoly x poly = eval x p where p = reverse poly
--eval x [y] = y
--eval x (p:ps) = p * (x ^ length ps) + eval x ps

evalPoly x [y] = y
evalPoly x (p:ps) = p + (x * (evalPoly x ps))   -- 1st num + x times p number of times = coefficient

