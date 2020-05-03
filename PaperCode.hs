 --HIGHER ORDER FUNCTIONS
--data List a = Empty | Cons a (List a) deriving (Show, Read, Eq, Ord)

[1,2,3] = 1:2:3:[]

sum' :: (Num a) => [a] -> a
sum' [] = 0
sum' (x:xs) = x + sum' xs

sum2 :: (Num a) => [a] -> a
sum2 = foldr (+) 0

product' xs = foldr (*) 1 xs 
anyTrue xs = foldr (||) False xs 
allTrue xs = foldr (&&) True xs

append xs ys = foldr (:) ys xs 

--length xs = foldr count' 0 xs
    --where count' a xs = a + 1


doubleandcons :: (Num a) => a -> [a] -> [a]
doubleandcons n xs = (2*n):xs
double n = (2*n)

doubleall xs = foldr ((:) . double) [] xs
--OR
doubleall2 = map double 

data Tree a = EmptyTree | Node a (Tree a) (Tree a) deriving (Show, Read, Eq) --node, l ,r
--node is a datatype

singleton :: a -> Tree a 
singleton x = Node x EmptyTree EmptyTree

treeInsert :: (Ord a) => a -> Tree a -> Tree a
treeInsert x EmptyTree = singleton x --base case
treeInsert x (Node a left right )
    | x == a = Node x left right 
    | x < a  = Node a (treeInsert x left) right 
    | x > a  =  Node a left (treeInsert x right)

treeElem :: (Ord a) => a -> Tree a -> Bool
treeElem x EmptyTree = False
treeElem x (Node a left right)
    | x == a = True 
    | x < a = treeElem x left 
    | x > a = treeElem x right 

foldtree :: (t1 -> t -> t) -> t -> Tree t1 -> t
foldtree f acc EmptyTree = acc
foldtree f acc (Node a left right) = foldtree f (f a (foldtree f acc right)) left 

--let nums = [8,6,4,1,7,3,5]  
--let numsTree = foldr treeInsert EmptyTree nums
--foldtree (+) 0 numsTree

treeFromList :: (Foldable t, Ord a) => t a -> Tree a
treeFromList xs = foldr treeInsert EmptyTree xs

sumTree :: (Num t) => Tree t -> t
sumTree tree = foldtree (+) 0 tree

productTree :: (Num t) => Tree t -> t
productTree tree = foldtree (*) 1 tree

mapTree :: (a -> b) -> Tree a -> Tree b
mapTree f EmptyTree = EmptyTree
mapTree f (Node a left right) = Node (f a) (mapTree f left) (mapTree f right)

-- The depth of a binary tree.

depthT :: Tree a -> Integer
depthT EmptyTree            = 0
depthT (Node n left right) = 1 + max (depthT left) (depthT right)

-- Turning a tree into a list.

collapse :: Tree a -> [a]
collapse EmptyTree = []
collapse (Node a left right) = collapse left ++ [a] ++ collapse right

--LAZY EVALUATION

--Newton raphson square root,  x=n^0.5 and x is close to the actual answer

next n x = (x + n/x)/2

--repeat' :: (Num a) => (a -> a) -> a -> [a]
repeat' f a = a: repeat' f (f a)

within :: (Ord a, Num a) => a -> [a] -> a
within eps (x:y:ys)
    | abs(x - y) <= eps  = y
    | otherwise        = within eps (y:ys)

squareRoot :: (Ord a, Fractional a) => a -> a -> a -> a
squareRoot a eps n = within eps (repeat' (next n) a)

--use a better distance between two terms 
relative :: (Ord a, Fractional a) => a -> [a] -> a
relative eps (x:y:ys)
    | abs(x/y - 1) <= eps  = y
    | otherwise        = relative eps (y:ys)

squareRoot2 :: (Fractional a, Ord a) => a -> a -> a -> a
squareRoot2 a eps n = relative eps (repeat' (next n) a)

--numerical differentiation
easydiff :: Fractional a => (a -> a) -> a -> a -> a
easydiff f x h = (f (x+h) - f x)/h

halve x = x/2

--derivative at any point x
differentiate :: Fractional b => b -> (b -> b) -> b -> [b]
differentiate h0 f x = map (easydiff f x) (repeat' halve h0)

--within eps (differentiate h0 f x)!!

elimerror :: (Fractional a, Integral t) => t -> [a] -> [a]
elimerror n (x:y:ys) = ((y * (2^n) - x)/(2^n - 1)): (elimerror n (y:ys))
--hard to know the right value of n, n?

order :: (RealFrac a, Integral b, Floating a) => [a] -> b
order(x:y:z:zs) = round(logBase (2) ((x - z)/(y - z) - 1))

--general function to improve a sequence of approximations

improve :: (Floating a, RealFrac a) => [a] -> [a]
improve s = elimerror (order s) s

--derivative computed more efficiently 
-- within eps (improve (differentiate h0 f x))!!

--within 0.000001 (differentiate 0.001 (\x -> x^2) 4)
--within 0.000001(improve (differentiate 0.001 (\x -> x^2) 4))
--within 0.000001(improve (improve (improve (differentiate 0.001 (\x -> x^2) 4))))

second :: [t] -> t
second (x:y:ys) = y

super :: (RealFrac b, Floating b) => [b] -> [b]
super s = map second (repeat' improve s)

--best approx: within eps (super (differentiate h0 f x))

--numerical integration 
easyintegrate f a b = ((f a + f b) * (b-a))/2

zip2 (x:xs) (y:ys) = (x,y): zip2 xs ys

addpair :: (Num a) => [(a, a)] -> [a]
addpair ((x,y):xs) = (x+y):addpair xs
addpair [] = []


integrate f a b = (easyintegrate f a b): (map addpair' (zip2 (integrate f a mid) (integrate f mid b)))
    where mid = (a+b)/2
          addpair' = (\(x,y) -> x+y)
          

-- bad becasue it recomputes the values of f
--this version never recomputes the values of f

integ f a b fa fb = 
    (easyintegrate f a b):(map addpair' (zip2 (integ f a m fa fm) (integ f m b fm fb))) --fb and fa are not evaluated here 
    where m = (a + b)/2
          fm = f m
          addpair' = (\(x,y) -> x+y)

integrate' f a b = integ f a b (f a) (f b)

--within eps (integrate' f a b)
--relative eps (integrate' f a b)
--super(integrate' sin 0.4)

--improve(integrate' f a b), use a lambda function for f

--------------------------------------------------------------
--alpha beta heuristic, from ai

--moves :: position -> listofposition 

--reptree f a = Node a (map (reptree f) (f a))

--gametree p = reptree moves p

--static :: position -> number 

--maximize (Node n sub) = max (map minimize sub)
--minimize (Node n sub) = min (map maximize sub)

--maximize (Node n []) = n
--maximize (Node n sub) = max(map minimize sub)
--minimize (Node n []) = n
--minimize (Node n sub) = max(map maximize sub)

--evaluate = maximize . maptree . static . gametree

--prune 0 (Node a x) = Node a []
--prune (n+1) (Node a x) = Node a (map(prune n) x)

--evaluate = maximize . maptree static . prune 5 . gametree

--maximize = max . maximize'

--maximize' (Node n []) = n:[]
--maximize' (Node n l) = map minimize l
                    -- = map (min minimize ')l
                    -- = map min (map minimize' l)
                    -- = mapmin (map minimize' l)
                    --where mapmin = map min

--mapmin (nums:rest) = (min nums):(omit (min nums) rest)

--omit pot [] = []
--omit pot (nums:rest) 
    -- | minleq nums pot = omit pot rest,
    -- | otherwise = (min nums): (omit(min nums) rest)

--minleq [] pot = False
--minleq(n:rest) pot
    -- | n<= pot = True
    -- | otherwise = minleq rest pot

--evaluate = max . maximize' . maptree static . prune 8 . gametree

--highfirst (Node n sub) = Node n (sort higher (map lowfirst sub))
--lowfirst (Node n sub) = Node n (sort (not . higher) (map highfirst sub))
--higher (Node n1 sub1) (Node n2 sub2) = n1 > n2

--evaluate = max . maximize' highfirst . maptree static . prune 8 . gametree

--taketree n = foldtree (nodett n) []:
--nodett n label sub = Node label (take n sub)

--prune 0 (Node pos sub)
    -- | dynamic pos = Node pos (map (prune 0) sub)

mergeSort :: (Ord a) => [a] -> [a]
mergeSort [] = []
mergeSort [x] = [x]
mergeSort xs = merge (mergeSort start) (mergeSort end)
    where start = take ((length xs) `div` 2) xs
          end = drop ((length xs) `div` 2) xs

merge :: (Ord a) => [a] -> [a] -> [a]
merge x [] = x
merge [] x = x
merge (x:xs) (y:ys)
    | x < y     = x:(merge xs (y:ys))
    | otherwise = y:(merge (x:xs) ys)