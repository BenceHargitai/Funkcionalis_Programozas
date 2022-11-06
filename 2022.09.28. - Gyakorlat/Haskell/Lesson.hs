module Lesson where

evens  :: Int -> Int -> [Int]
evens x y = [z | z<- [(x+1)..(y-1)], even z]

dominos :: [(Int, Int)]
dominos = [(x,y) | x<-[0..6], y<-[0..6], x>=y]

