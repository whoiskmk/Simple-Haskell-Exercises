{-# OPTIONS_GHC -Wall #-}
module Lab10 where

newtype Poly a = P [a]

-- Exercise 1 -----------------------------------------

x :: Num a => Poly a
x = P[0,1];

-- Exercise 2 ----------------------------------------

equals :: (Num a, Eq a) => Poly a -> Poly a -> Bool 
equals (P []) (P [])=True 
equals (P (x:xs)) (P [])
    | (x/=0) =False
    |otherwise= equals (P xs) (P []) 
equals (P []) (P (y:ys)) 
    | (y/=0) =False
    |otherwise= equals (P []) (P ys) 
equals (P (x:xs)) (P (y:ys))
    | (x/=y)=False
    |otherwise= equals (P xs) (P ys)

instance (Num a, Eq a) => Eq (Poly a) where
    (==) = equals



-- Exercise 3 -----------------------------------------

term :: (Num a, Show a, Eq a) => a -> Int -> String
term 0 _ = ""
--coefficent is 0
term b 0 = show b
--coefficent is b, exponent is 0
term 1 1 = "x"
--coefficent is 1, exponent is 1
term (-1) 1 = "-x"
--coefficent is -1, exponent is 1
term 1 b = "x^" ++ (show b)
--coefficent is 1, exponent is b
term (-1) b = "-x^" ++ (show b)
--coefficent is -1, exponent is b
term b 1 = (show b) ++ "x"
--coefficent is b, exponent is 1
term b c = (show b) ++ "x^" ++ (show c)
--coefficent is b, exponent is c

--helper :: (Num a, Show a, Eq a) => [a] -> [a] -> String
--helper xs ys = foldl (++) "" (zipWith term xs ys)

helper :: [String] -> String
helper [] = "0"
--empty list will be equal to zero
helper [""] = "0"
--empty list will be equal to zero
helper [s] = s
--string with just one integer will return the constant of the integer
helper ("":ss) = helper ss
--sperating nothing from the rest of the list will jsut return the rest of the list
helper (s:ss) = (helper ss) ++ " + " ++ s
--calls helper on all but head of list and also returns first element as constant

instance (Num a, Eq a, Show a) => Show (Poly a) where
	--show P 
    show (P xs) = helper (zipWith term xs [0..])
    -- will call hlper on each term of zipwith



-- Exercise 4 -----------------------------------------

plus :: Num a => Poly a -> Poly a -> Poly a
plus (P x) (P y)
    |(length x==length y)=P (zipWith (+) (x) (y))
    |(length x>length y)= P (zipWith (+) (x) (y++repeat 0))
    |otherwise = P (zipWith (+) (x++repeat 0)  (y))


-- Exercise 5 -----------------------------------------


times :: Num a => Poly a -> Poly a -> Poly a
--takes in two (Poly a)s that is type Num a and returns Poly a
times (P xs) (P ys) = foldr (+) (P [0]) (zipWith (\b c -> P((replicate c 0)++b)) (map (\i -> map (i*) ys) xs) [0..])
-- this takes a polynomial and multiplies each item by each item of the other polynomial it takes in 


-- Exercise 6 -----------------------------------------

instance Num a => Num (Poly a) where
    (+) = plus
    (*) = times
    negate (P [])=0
    negate (P x)= times (P x) (P[(-1)])  
   fromInteger c =  (P [fromInteger c])
    -- No meaningful definitions exist
    abs    = undefined
    signum = undefined

-- Exercise 7 -----------------------------------------
changeList :: [Integer]->Integer -> [Integer]
changeList [] m = []
changeList (n:ns) m = (m * n) : changeList ns m


applyP :: Num a => Poly a -> a -> a
applyP (P []) m=0
--applyP  (P (x:xs)) m=  x + (applyP (P (changeList xs m)) m)
applyP  (P (x:xs)) m=  x + m* (applyP (P xs) m)