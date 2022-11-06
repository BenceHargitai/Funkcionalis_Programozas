module Lesson where

replaceAll :: Eq a => a-> a -> [a] -> [a]
replaceAll _ _ [] = []
replaceAll a x (b:bs) 
    | a == b = x : replaceAll a x bs
    | otherwise = b : replaceAll a x bs

repeat' :: a -> [a]
repeat' x = x : repeat x

take' :: Int -> [a] -> [a]
take' 0 l = []
take' n _ | n < 0 = []
take' x [] = []
take' x (y:ys) = y : take' (x-1) ys

drop' :: Int -> [a] -> [a]
drop' 0 l = l
drop' x [] = []
drop' x (y:ys) = drop' (x-1) ys

langAndRegion :: String -> (String, String)
langAndRegion s = (take 2 s, drop 3 s)

unzip' :: [(a,b)] -> ([a], [b])
unzip' [] = ([], [])
unzip' ((a,b):xs) = (a:as, b:bs) where (as,bs) = unzip' xs  


isEmpty :: String -> Bool
isEmpty y 
    | y =="" = True
    | otherwise = False


emptyLines :: String -> [Int]
emptyLines s = [i | (i, x) <- zip [0..] (lines s), isEmpty x]




    
