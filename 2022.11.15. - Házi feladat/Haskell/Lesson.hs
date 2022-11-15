module Homework7 where

data Vector3 = V Int Int Int
    deriving (Show, Eq)

componentSum :: Vector3 -> Int
componentSum (V a b c) = a + b + c

crossProduct :: Vector3 -> Vector3 -> Vector3
crossProduct (V a1 a2 a3) (V b1 b2 b3) = (V (a2*b3 - a3*b2) (a3*b1 - a1*b3) (a1*b2 - a2*b1))

vectorListSum :: [Vector3] -> Vector3
vectorListSum [] = V 0 0 0
vectorListSum (V x y z : xs) = V (x + x1 ) (y + y1) (z + z1) where V x1 y1 z1 = vectorListSum xs

data Storage = HDD String Int Int | SSD String Int
    deriving (Show, Eq)

capacity :: Storage -> Int
capacity (SSD a b) = b
capacity (HDD a b c) = c

isHDD :: Storage -> Bool
isHDD (SSD a b) = False
isHDD (HDD a b c) = True