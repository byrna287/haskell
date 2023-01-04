-- lab 5

data BinTree t = Empty | Root t (BinTree t) (BinTree t)
                 deriving (Eq, Ord, Show)

leaf x = Root x Empty Empty

addnode :: Ord a => a -> BinTree a -> BinTree a

addnode a Empty = leaf a
addnode a (Root x left right) = if a < x
                                then Root x (addnode a left) right
                                else Root x left (addnode a right)


maketree :: Ord a => [a] -> BinTree a

maketree [] = Empty
maketree [x] = leaf x
maketree (x:xs) = addnode x (maketree xs)


inorder :: BinTree a -> [a] 

inorder Empty = []
inorder (Root x Empty Empty) = [x]
inorder (Root x left right) = inorder left ++ [x] ++ inorder right

mpsort :: Ord a => [a] -> [a]

mpsort x = inorder (maketree x)


