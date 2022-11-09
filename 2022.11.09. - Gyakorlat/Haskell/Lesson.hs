module Lesson where

type Point = (Int, Int)

p :: Point
p = (0,0)

newtype Point' = P (Int,Int)
    deriving (Show)

data Point'' = P' Int Int | D Double Double
    deriving (Show)

data Day = Mon | Tue | Wed | Thue | Fri | Sat | Sun

isFirstDayOfWeek :: Day -> Bool
isFirstDayOfWeek Mon = True
isFirstDayOfWeek _ = False

data Time = T Int Int

showTime :: Time -> String
showTime (T h m) = show h ++ ":" ++ show m

eqTime :: Time -> Time -> Bool
eqTime (T h m ) (T h1 m1) = h==h1 && m == m1
    
isEarlier :: Time -> Time -> Bool
isEarlier (T h m ) (T h1 m1) = h==h1 && m < m1 || h<h1

isBetween :: Time -> Time -> Time -> Bool
isBetween (T h m ) (T h1 m1) (T h2 m2) = 
    (h*60+m) < (h1*60+m1) && (h1*60+m1) < (h2*60+m2) || 
    (h*60+m) > (h1*60+m1) && (h1*60+m1) > (h2*60+m2) 