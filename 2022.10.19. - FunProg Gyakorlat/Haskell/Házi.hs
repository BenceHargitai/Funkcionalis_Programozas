module Homework5 where
import Data.Char

geometricSequence :: Num a => a -> a -> [a]
geometricSequence a r = a : geometricSequence (a * r) r


isSorted :: Ord a => [a] -> Bool
isSorted [] = True
isSorted [x] = True
isSorted (x:y:xs) = x <= y && isSorted (y:xs)

-- fromTo :: Int -> Int -> [a] -> [a]
-- fromTo _ _ [] = []
-- fromto a b l = (drop b l) (take b-a l)

-- * lambda fv: névtelen
-- * \x y -> fv törzs

-- ? Magasabbrendű fv.:
-- * visszatérési értékként fv.t ad vissza vagy bemenő paraméterként fv-t vár el, haskellben ez mindenre teljesülne
-- * map :: (a->b) -> [a] -> [b]

map' :: (a->b) -> [a] -> [b]
map' _ [] = []
map' f (x:xs) = f x : map' f xs

filter' :: (a->Bool) -> [a] -> [a]
filter' _ [] = []
filter' f (x:xs)
    | f x = x : filter' f xs
    | otherwise = filter' f xs


upperToLower ::String -> String
upperToLower "" = ""
upperToLower (x:xs)
    | isUpper x =  toLower x : upperToLower xs
    | otherwise = upperToLower xs


all' :: (a->Bool) -> [a] -> Bool
all' _ [] = True
all' f (x:xs)
    | f x = all' f xs
    | otherwise = False

any' :: (a->Bool) -> [a] -> Bool
any' _ [] = False
any' f (x:xs)
    | not (f x) = any' f xs
    | otherwise = True


hasLongLines :: String -> Bool
hasLongLines "" = False
hasLongLines s = any' (\x -> length x >= 3) (words s)

elem' :: Eq a => a -> [a] -> Bool
elem' c [] = False
elem' c s = any' (\x -> x == c) s

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny s [] = False
hasAny [] s = False
hasAny (x:xs) s = any' (\y -> y == x) s

takeWhile' :: (a->Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f (x:xs)
    | f x = x : takeWhile' f xs
    | otherwise = []

dropWhile' :: (a->Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f (x:xs)
    | f x = dropWhile' f xs
    | otherwise = x:xs