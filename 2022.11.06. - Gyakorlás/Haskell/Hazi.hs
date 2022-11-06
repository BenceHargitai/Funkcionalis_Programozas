module Lesson where
import Data.List

rook :: (Int, Int) -> [(Int, Int)] 
rook (x, y) = [(x, y') | y' <- [0..7],y'/=y ] ++ [(x', y) | x' <- [0..7], x' /= x]

knight :: (Int, Int) -> [(Int, Int)]
knight (x, y) = [(a,b) | a <- [0..7], b <- [0..7], abs (x-a) + abs (y-b) == 3, a/=x, b/=y]

attacks :: ((Int, Int) -> [(Int, Int)]) -> (Int, Int) -> [(Int, Int)] -> Bool
attacks f (a,b) [] = False
attacks f (a,b) (l:ls) = any (True==)  (elem l (f (a,b)) : [attacks f (a,b) ls])


