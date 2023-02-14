--Page 60--
--Exercise 1 page 60 Real world haskell
--List type
data List a = Cons a (List a)
    | Nil
    deriving (Show)

toList :: List a -> [a]
toList (Cons a xs) = a:toList xs
toList Nil = [] 

--Exercise 2 page 60 Real world haskell
--Here is the construction of a simple tree
--ghci> Node 1 (Just (Node 2 Nothing Nothing)) (Just (Node 3 Nothing Nothing))
--data Tree a = Node a (Maybe (Tree a)) (Maybe (Tree a))
--            deriving (Show)

--Page 69--
--Exercise 1, 2
--Alternative length function
length' :: (Num b) => [a] -> b
length' [] = 0
length' (x:xs) = 1 + length' xs 

--Exercise 3
--Calculating mean
mean :: (Fractional a) => [a] -> a
mean [] = 0
mean li = (sum li) / (fromIntegral (length li))

--Exercise 4 To palindrome
reverse' :: [a] -> [a]
reverse' []   = []
reverse' li   = (last li):(reverse' (init li))

toPalindrome :: [a] -> [a]
toPalindrome [] = []
toPalindrome li = li ++ tail (reverse' li)

--Exercise 5 isPalindrome
isPalindrome :: Eq a => [a] -> Bool
isPalindrome [] = False
isPalindrome li = (take partLen li) == reverse' (drop (partLen + 1) li)
    where
      partLen = (fromIntegral (length li) - 1) `div` 2

--Exercise 6 Sort by list length
--Sort a list of lists by length
--sortByLength [[1,2,3,4],[1,2,3],[1]]
--will be [[1],[1,2,3],[1,2,3,4]]
--This can be better I guess
isAsc :: [Int] -> Bool
isAsc [] = False
isAsc [x] = True -- If looped until the end
isAsc (x:xs) = x <= head xs && isAsc xs


--bubblesort :: (Ord a) => [a] -> [a]
bubblesort []  = []
bubblesort [a] = [a]
bubblesort li  = if not (isAsc li) then bubblesort (aux li) else li
    where 
      aux [] = []
      aux [x] = [x]
      aux (x:y:xs)
        | x > y = y:(aux (x:xs))
        | otherwise = x:(aux (y:xs))

elem' :: (Eq a) => a -> [a] -> Bool
elem'  _ [] = False
elem' a (x:xs)  = (a == x) || elem' a xs

nub :: (Eq a) => [a] -> [a]
nub [] = []
nub (x:xs) = 
    if elem' x xs
    then nub xs
    else x:nub xs

sortByLength :: [[a]] -> [[a]]
sortByLength li = concat (map (\x -> filter (\y -> (length y) == x) li) sLi)
    where
      sLi = nub(bubblesort (map (\x -> length x) li))

--Exercise 7,8
--What this does is join a list together with a separator value
--Example:
--ghci> intersperse ',' []
--""
--ghci> intersperse ',' ["foo"]
--"foo"
--ghci> intersperse ',' ["foo","bar","baz","quux"]
--"foo,bar,baz,quux"
intersperse :: a -> [[a]] -> [a]
intersperse _ [] = []
intersperse a [x,y] = x ++ a:[] ++ y
intersperse a (x:xs) = x ++ a:[] ++ (intersperse a xs)

--Exercise 9
data Tree a = Node a (Tree a) (Tree a)
            | Empty
            deriving (Show, Eq)

--DFT
--The height of this binary tree 3
-- 1
-- l 2 l 4
--     r 5
-- r 3 l null
--     r 6 l 7
--         r null
-- In argument form
--  (Node 1 (Node 2 (Node 4 Empty Empty) (Node 5 Empty Empty)) (Node 3 (Empty) (Node 6 (Node 7 Empty Empty)(Empty))))
treeHeightDepthFirst  Empty = 0
treeHeightDepthFirst  (Node a b c)
    | b == Empty && c == Empty = 1
    | otherwise = 1 + (max (treeHeightDepthFirst b) (treeHeightDepthFirst c))

--Exercise 10
--For 3 2d points, construct a data type that will tell if it's goings left right or going straight.
--D for direction
--
--Author:
--https://stackoverflow.com/questions/18723381/rounding-to-specific-number-of-digits-in-haskell
truncate' :: Double -> Int -> Double
truncate' x n = (fromIntegral (floor (x * t))) / t
    where t = 10^n

data Point = Point (Double, Double)
            deriving (Show)

data Vector = Vector (Double, Double)
            deriving (Show)

data Direction = DLeft | DRight | DStraight 
               | DUnknown
               deriving (Show)

directionVector ((x1, y1), _ , (x2, y2)) = Vector (x2 - x1, y2 - y1)
normVector :: Vector -> Double
normVector (Vector (x, y)) = sqrt(x^2 + y^2)
unitVector :: Vector -> Vector
unitVector (Vector (x, y))  = Vector ((x * invVNorm), (y * invVNorm))
    where
      invVNorm = (1/normVector(Vector (x, y)))

--point2Direction :: [(Point, Point, Point)] -> [Direction]
--point2Direction [] = [DUnknown]
--point2Direction [(a,b,c)] = [DLeft]
