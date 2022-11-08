module Lesson where
import Data.Char

map' :: (a -> b) -> [a] -> [b]
map' f [] = []
map' f l = [f x | x <- l]

filter' :: (a -> Bool) -> [a] -> [a]
filter' f [] = []
filter' f l = [x | x <- l, f x]

upperToLower :: String -> String
upperToLower = filter' isUpper

--all function without list and with header guards
all' :: (a -> Bool) -> [a] -> Bool
all' f [] = True
all' f l 
    | length (filter' f l) == length l = True
    | otherwise = False 

all'' :: (a -> Bool) -> [a] -> Bool
all'' f [] = True
all'' f l = foldr (\x acc -> f x && acc) True l 

any' :: (a -> Bool) -> [a] -> Bool
any' f [] = False
any' f l 
    | length (filter' f l) > 0 = True
    | otherwise = False

hasLongLines' :: String -> Bool
hasLongLines' s = any' (\x -> length (words x) > 3) (lines s)

elem' :: Eq a => a -> [a] -> Bool
elem' c [] = False
elem' c l = any' (\x -> x == c) l

hasAny :: Eq a => [a] -> [a] -> Bool
hasAny [] _ = False
hasAny _ [] = False
hasAny f s = any' (\x -> elem' x s) f

--takeWhile with high order function
takeWhile' :: (a -> Bool) -> [a] -> [a]
takeWhile' f [] = []
takeWhile' f l= foldr (\x acc -> if f x then x:acc else []) [] l

dropWhile' :: (a -> Bool) -> [a] -> [a]
dropWhile' f [] = []
dropWhile' f l
    | f (head l) = dropWhile' f (drop 1 l)
    | otherwise = l

dropWord :: String -> String
dropWord "" = ""
dropWord s = dropWhile'(\x -> x /= ' ') s

users :: [(String, String)]
users = [ ("mrbean", "4321"), ("admin", "s3cr3t"), ("finn", "algebraic")]

doesUserExist :: String -> [(String,String)] -> Bool
doesUserExist "" l = False
doesUserExist s [] = False
doesUserExist s l = any' (\x -> fst x == s) l

dropSpaces :: String -> String
dropSpaces "" = ""
dropSpaces s = dropWhile'(\x -> x == ' ') s

trim :: String -> String
trim "" = ""
trim s = reverse $ dropSpaces $ reverse $ dropSpaces s

monogram :: String -> String
monogram "" = ""
monogram 