module Lesson where
import Graphics.Win32 (rectangle)

type RAM                    = Int
type DriveCapacity          = Int
type BatteryOperatingTime   = Int
type PoweredOn              = Bool

data Manufacturer = Intel | AMD
    deriving(Show, Eq)

data Computer = PC Manufacturer RAM DriveCapacity PoweredOn | Laptop Manufacturer RAM DriveCapacity BatteryOperatingTime PoweredOn
    deriving(Show, Eq)

type ComputerRoom = [Computer]

changeToIntel :: [Computer] -> [Computer]
changeToIntel [] = []
changeToIntel ((PC cpu ram dc po):xs)
    | not (po) = (PC Intel ram dc po) : changeToIntel xs
    | otherwise = (PC cpu ram dc po) : changeToIntel xs
changeToIntel ((Laptop cpu ram dc b po):xs)
    | not (po) = (Laptop Intel ram dc b po) : changeToIntel xs
    | otherwise = (Laptop cpu ram dc b po) : changeToIntel xs

buildComputerRoom :: [Maybe Computer] -> Maybe ComputerRoom
buildComputerRoom [] = []
buildComputerRoom Nothing = Nothing
buildComputerRoom (Just (PC cpu ram dc po):xs) = (PC cpu ram dc po) : buildComputerRoom xs
buildComputerRoom (Just (Laptop cpu ram dc b po):xs) = (Laptop cpu ram dc b po) : buildComputerRoom xs














{-
data Rectangle = Rectangle Int Int Int Int
    deriving(Show,Eq)

rectangleArea :: Rectangle -> Maybe Int
rectangleArea (Rectangle x1 y1 x2 y2) 
    | x1 > x2 || y1 > y2 = Nothing
    | otherwise = Just ((x2 - x1) * (y2-y1))


noOverlappingRectangles :: [Rectangle] -> Bool
noOverlappingRectangles [] = True
noOverlappingRectangles  (Rectangle x1 y1 x2 y2 :xs) = noOverlappingRectangles xs && (all (\(Rectangle x3 y3 x4 y4) -> (x1 < x4) || (x2 > x3) || (y1 < y4) || (y2 > y3)) xs)
-}