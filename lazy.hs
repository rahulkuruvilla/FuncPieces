--Newton raphson square root,  x=n^0.5 and x is close to the actual answer
next n x = (x + n/x)/2

repeat' :: (Num a) => (a -> a) -> a -> [a]
repeat' f a = a: repeat' f (f a)

within :: (Ord a, Num a) => a -> [a] -> a
within eps (x:y:ys)
    | abs(x - y) <= eps  = y
    | otherwise        = within eps (y:ys)


use a better distance between two terms 
relative :: (Ord a, Fractional a) => a -> [a] -> a
relative eps (x:y:ys)
    | abs(x/y - 1) <= eps  = y
    | otherwise        = relative eps (y:ys)



squareRoot :: (Ord a, Fractional a) => a -> a -> a -> a
squareRoot a eps n = within eps (repeat' (next n) a)

squareRoot2 :: (Fractional a, Ord a) => a -> a -> a -> a
squareRoot2 a eps n = relative eps (repeat' (next n) a)

--numerical differentiation
easydiff :: Fractional a => (a -> a) -> a -> a -> a
easydiff f x h = (f (x+h) - f x)/h
halve x = x/2

--derivative at any point x
differentiate :: Fractional b => b -> (b -> b) -> b -> [b]
differentiate h0 f x = map (easydiff f x) (repeat' halve h0)

elimerror :: (Fractional a, Integral t) => t -> [a] -> [a]
elimerror n (x:y:ys) = ((y * (2^n) - x)/(2^n - 1)): (elimerror n (y:ys))
--hard to know the right value of n, n?

order :: (RealFrac a, Integral b, Floating a) => [a] -> b
order(x:y:z:zs) = round(logBase (2) ((x - z)/(y - z) - 1))

--general function to improve a sequence of approximations
improve :: (Floating a, RealFrac a) => [a] -> [a]
improve s = elimerror (order s) s

--derivative computed more efficiently 
within eps (improve (differentiate h0 f x))!!
within 0.000001 (differentiate 0.001 (\x -> x^2) 4)
within 0.000001(improve (differentiate 0.001 (\x -> x^2) 4))
within 0.000001(improve (improve (improve (differentiate 0.001 (\x -> x^2) 4))))

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