maximum' :: (Ord a) => [a] -> a
maximum' [] = error "Maximum of empty list"
maximum' [x] = x
maximum' (x:xs) = max x (maximum' xs)
--------------------------------------------------------
replicate' :: Int -> a -> [a]
replicate' n x
    |n<=0 = []
    |otherwise = x:replicate' (n-1) x
--------------------------------------------------------
repeat' :: a -> [a]
repeat' x = x:repeat' x
--------------------------------------------------------
quicksort :: (Ord a) => [a] -> [a]
quicksort [] = []
quicksort [x] = [x]
quicksort (x:xs) = 
    let smalloreq = [a | a <- xs, a<=x]
        larger = [a | a <- xs, a>x]
    in quicksort smalloreq ++ [x] ++ quicksort larger
--------------------------------------------------------
factorial :: Integer -> Integer
factorial 0 = 1
factorial n = n * factorial (n-1)
--------------------------------------------------------
fibonacci :: Integer -> Integer  
fibonacci 0 = 0
fibonacci 1 = 1
fibonacci n = fibonacci (n-1) + fibonacci (n-2)
--------------------------------------------------------
rev_list :: [a] -> [a]
rev_list [] = []
rev_list (xs) = [last (xs)] ++ rev_list (init xs) 
--------------------------------------------------------
init' :: [a] -> [a]
init' [] = []
init' [x] = []
init' (x:xs) = x : init'(xs)
--------------------------------------------------------
head' :: [a] -> a
--What is the base condition here?
head' [] = error "Empty List"
head' (x:xs) = x 
--------------------------------------------------------
slice ::  [a] -> Int -> Int -> [a] 
slice (x) start_index end_index 
    |start_index<0 = error "Index error"
    |end_index>length(x) = error "Index out of bound"
--------------------------------------------------------
sum_of_digits :: Integer -> Integer
sum_of_digits 0 = 0
sum_of_digits n = (n `mod` 10) + (sum_of_digits (n `div` 10))
--------------------------------------------------------
tail' :: [a] -> [a]
tail' [] = []
tail' (x:xs) = xs
--------------------------------------------------------
-- last' :: [a] -> a
-- last' [x] = x
-- last' (x:xs) = []: last'(xs)
--------------------------------------------------------
length' :: [a] -> Integer
length' [] =  0
length' (x:xs) = 1 + length' (xs)
--------------------------------------------------------
null' :: [a] -> Bool
null' [] = True
null' (x:xs) = False
--------------------------------------------------------
zipWith' ::  (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' _ [] _ = []
zipWith' _ _ [] = []
-- after ":" there should be a number but after "++" there should be a list
-- Remember a string is also a list (list of characters) 
zipWith' f (x:xs) (y:ys) = f x y : zipWith' f xs ys
--------------------------------------------------------
-- flip' :: (a->b->c) -> b -> a -> c
-- flip' f a b = b f a
--flip,map,filter