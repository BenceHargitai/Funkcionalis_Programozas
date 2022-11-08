module Lesson where
import Data.Char
import Data.List

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
monogram s = unwords $ map (\(x:xs) -> x : ".") (words s)

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq l = map(\(x:xs) -> x) $ group $ sort l 

repeated :: Ord a => [a] -> [a]
repeated [] = []
repeated l = map(\(x:xs) -> x) $ filter(\y -> length y > 1) $ group $ sort l

zipWith' :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWith' f [] list2 = []
zipWith' f list1 [] = []
zipWith' f (a:as) (b:bs) = f a b : zipWith' f as bs

-- --zipwith high order function
-- zipWith'' :: (a -> b -> c) -> [a] -> [b] -> [c]
-- zipWith'' f [] list2 = []
-- zipWith'' f list1 [] = []
-- zipWith'' f list1 list2 = foldr (\(x,y) acc -> f x y : acc) [] (zip list1 list2)

dotProduct :: [Int] -> [Int] -> Int
dotProduct [] l2 = 0
dotProduct l1 [] = 0
dotProduct l1 l2 = sum $ zipWith' (*) l1 l2

isPrime :: Int -> Bool
isPrime 1 = False
isPrime a 
    | length (filter(\x -> x == True) (map(\x -> a `mod` x == 0) [2..a-1])) == 0 = True 
    | otherwise = False

primes :: Int -> [Int]
primes x = [l | l <- [2..x], isPrime l]

iterate'' :: (a -> a) -> a -> [a] 
iterate'' f x = x : iterate'' f (f x)

fibonacci :: [Int]
fibonacci = 0 : 1 : zipWith' (+) fibonacci (tail fibonacci)

