module Homework5 where


geometricSequence :: Num a => a -> a -> [a]
geometricSequence a r = a : geometricSequence (a * r) r

isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

runs :: Int -> [a] -> [[a]]
runs n [] = []
runs n l = take n l : runs n (drop n l)

fromTo :: Int -> Int -> [a] -> [a]
fromTo _ _ [] = []
fromTo 0 0 (x:xs) = []
fromTo 0 n (x:xs) = x : fromTo 0 (n-1) xs
fromTo m n (x:xs) = fromTo (m - 1) (n - 1) xs

