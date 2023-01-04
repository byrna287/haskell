-- lab 4

myAppend :: [a] -> [a] -> [a]

myAppend x [] = x
myAppend [] x = x
myAppend (x:xs) y = (x: myAppend xs y)

myHead :: [a] -> a

myHead [] = error "List must be non-empty"
myHead (x:xs) = x

myLast :: [a] -> a

myLast [] = error "List must be non-empty"
myLast [x] = x
myLast (x:xs) = myLast xs

myTail :: [a] -> [a]

myTail [] = error "List must be non-empty"
myTail (x:xs) = xs

myInit :: [a] -> [a]

myInit [] = error "List must be non-empty"
myInit x = reverse (myTail (reverse x))

myLength :: [a] -> Int

myLength [] = 0
myLength [x] = 1
myLength (x:xs) = 1 + myLength xs

myReverse :: [a] -> [a]

myReverse [] = []
myReverse [x] = [x]
myReverse (x:xs) = myAppend (myReverse xs) [x]

myConcat :: [[a]] -> [a]

myConcat [] = []
myConcat [x] = x
myConcat (x:xs) = myAppend x (myConcat xs)

mySum :: Num a => [a] -> a

mySum [] = 0
mySum [x] = x
mySum (x:xs) = x + mySum xs

myProduct :: Num a => [a] -> a

myProduct [] = 0
myProduct [x] = x
myProduct (x:xs) = x * myProduct xs

myMaximum :: Ord a => [a] -> a

myMaximum [] = error "List must be non-empty"
myMaximum [x] = x
myMaximum (x:xs) = if x > biggest
                   then x
                   else biggest
                   where biggest = myMaximum xs

myMinimum :: Ord a => [a] -> a

myMinimum [] = error "List must be non-empty"
myMinimum [x] = x
myMinimum (x:xs) = if x < smallest
                   then x
                   else smallest
                   where smallest = myMinimum xs

myElem :: Eq a => a -> [a] -> Bool

myElem a [] = False
myElem a [x] = a == x
myElem a (x:xs) = if a == x
                  then True
                  else myElem a xs

myDelete :: Eq a => a -> [a] -> [a]

myDelete a [] = []
myDelete a [x] = if a == x
                 then []
                 else [x]
myDelete a (x:xs) = if a == x
                    then xs
                    else (x:myDelete a xs)

myUnion :: Eq a => [a] -> [a] -> [a]

myUnion [] [] = []
myUnion x [] = x
myUnion [] x = x
myUnion x (y:ys) = if myElem y x == False
                   then myUnion (myAppend x [y]) ys
                   else myUnion x ys

myIntersect :: Eq a => [a] -> [a] -> [a]

myIntersect [] [] = []
myIntersect x [] = []
myIntersect [] x = []
myIntersect (x:xs) y = if myElem x y == True
                       then (x:myIntersect xs y)
                       else myIntersect xs y



