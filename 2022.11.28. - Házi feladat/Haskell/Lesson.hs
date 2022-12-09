module Lesson where

-- =============================================================================
--                              models
-- =============================================================================
type Coordinate = (Int, Int)
type Sun = Int

data Plant = Peashooter Int | Sunflower Int| Walnut Int| CherryBomb Int
    deriving(Show, Eq)

data Zombie = Basic Int Int | Conehead Int Int | Buckethead Int Int | Vaulting Int Int
    deriving(Show, Eq)

data GameModel = GameModel Sun [(Coordinate, Plant)] [(Coordinate, Zombie)]
    deriving(Show, Eq)

defaultPeashooter :: Plant
defaultPeashooter = Peashooter 3

defaultSunflower :: Plant
defaultSunflower = Sunflower 2

defaultWalnut :: Plant
defaultWalnut = Walnut 15

defaultCherryBomb :: Plant
defaultCherryBomb = CherryBomb 2

basic :: Zombie
basic = Basic 5 1

coneHead :: Zombie
coneHead = Conehead 10 1

bucketHead :: Zombie
bucketHead = Buckethead 20 1

vaulting :: Zombie
vaulting = Vaulting 7 2

-- =============================================================================
--                              tryPurchase
-- =============================================================================

shoppingList :: [(Plant,Int)]
shoppingList = [(defaultPeashooter, 100), (defaultSunflower, 50), (defaultWalnut, 50), (defaultCherryBomb, 150)]

tryPurchase :: GameModel -> Coordinate -> Plant -> Maybe GameModel
tryPurchase (GameModel sun plants zombies) (x,y) plant
    | getPlantCost plant > sun = Nothing
    | (x,y) `elem` (map fst plants) = Nothing
    | x >= 5 || y >= 12 || x < 0 || y < 0 = Nothing
    | otherwise = Just (GameModel (sun - getPlantCost plant) (((x,y),plant):plants) zombies)

getPlantCost :: Plant -> Int
getPlantCost plant = snd (head (filter (\(x,y) -> x == plant) shoppingList))

-- =============================================================================
--                              placeZombieInLane
-- =============================================================================

placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plants zombies) zombie lane
    | lane >= 5 || lane < 0 = Nothing 
    | (lane, 11) `elem` (map fst zombies) = Nothing
    | otherwise = Just (GameModel sun plants (((lane,11),zombie):zombies))

-- =============================================================================
--                              performZombieActions
-- =============================================================================

