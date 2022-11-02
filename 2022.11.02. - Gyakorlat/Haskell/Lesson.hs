module Lesson where
import Data.Char

-- *? 10.gyakorlat

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


dropWord :: String -> String
dropWord "" = ""
dropWord s
    | length (words s) > 1 = dropWhile' (\x -> x /= ' ') s
    | otherwise = s

users :: [(String, String)]
users = [ ("mrbean", "4321"), ("admin", "s3cr3t"), ("finn", "algebraic")]

doesUserExist :: String -> [(String, String)] -> Bool
doesUserExist "" _ = False
doesUserExist _ [] = False
doesUserExist s (x:xs)
    | s == fst x = True
    | otherwise = doesUserExist s xs


-- !? 11. Gyakorlat

dropSpaces :: String -> String
dropSpaces s
    | take 1 s == " " = drop 1 s
    | otherwise = s

trim :: String -> String
trim "" = ""
trim s 
    | take 1 s == " " = trim (drop 1 s)
    | take 1 (reverse s) == " " = trim (reverse (drop 1 (reverse s)))
    | otherwise = s


dropSpaces' :: String -> String
dropSpaces' [] = []
dropSpaces' x = dropWhile' (==' ') x

monogram :: String -> String
monogram s = unwords (map (\(x:_) -> x: "." ) (words s ))

uniq :: Ord a => [a] -> [a]
uniq [] = []
uniq (x:xs) = x : uniq (filter (/= x) xs)

uniq' :: Ord a => [a] -> [a]
uniq' s = map head (group (sort s))

repeated :: Ord a => [a] -> [a]
repeated [] = []
repeated (x:xs) = x : repeated (filter (== x) xs)