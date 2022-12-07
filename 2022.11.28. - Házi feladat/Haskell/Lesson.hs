    module Lesson where

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


--Valamiért kiírja a két intet is nálam
placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plants zombies) zombie lane
    | lane >= 5 || lane < 0 = Nothing 
    | (lane, 11) `elem` (map fst zombies) = Nothing
    | otherwise = Just (GameModel sun plants (((lane,11),zombie):zombies))


performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel sun plants []) = Just (GameModel sun plants [])
performZombieActions (GameModel sun plants l@(((x,y),zombie):zombies))
    | y == 0 = Nothing
    | Vaulting a 2 <- zombie = Just (GameModel sun plants (((x,(changeY y 2)),Vaulting a 1):zombies))
    | plantHere x y plants = Just (GameModel sun (hitPlant x y plants) (((x,y),zombie):zombies))
    | otherwise = Just (GameModel sun plants (((x,y-1),zombie):zombies))

plantHere :: Int -> Int -> [(Coordinate, Plant)] -> Bool
plantHere x y plants = (x,y) `elem` (map fst plants)

hitPlant :: Int -> Int -> [(Coordinate, Plant)] -> [(Coordinate, Plant)]
hitPlant x y plants = map (\(coord, plant) -> if coord == (x,y) then (coord, hitPlant' plant) else (coord, plant)) plants

hitPlant' :: Plant -> Plant
hitPlant' (Peashooter health) = Peashooter (health-1)
hitPlant' (Sunflower health) = Sunflower (health-1)
hitPlant' (Walnut health) = Walnut (health-1)
hitPlant' (CherryBomb health) = CherryBomb (health-1)

changeY :: Int -> Int -> Int
changeY y steps
    | y - steps < 0 = 0
    | otherwise = y - steps


cleanBoard :: GameModel -> GameModel
cleanBoard (GameModel sun plants zombies) = GameModel sun (removePlant plants) (removeZombie zombies)

removePlant :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
removePlant [] = []
removePlant ((coord, plant):plants)
    | Peashooter health <- plant, health <= 0 = removePlant plants
    | Sunflower health <- plant, health <= 0 = removePlant plants
    | Walnut health <- plant, health <= 0 = removePlant plants
    | CherryBomb health <- plant, health <= 0 = removePlant plants
    | otherwise = ((coord, plant):removePlant plants)


removeZombie :: [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
removeZombie [] = []
removeZombie ((coord, zombie):zombies)
    | Basic 0 _ <- zombie = removeZombie zombies
    | Conehead 0 _ <- zombie = removeZombie zombies
    | Buckethead 0 _ <- zombie = removeZombie zombies
    | Vaulting 0 _ <- zombie = removeZombie zombies
    | otherwise = ((coord, zombie):removeZombie zombies)

performPlantActions :: GameModel -> GameModel
performPlantActions (GameModel sun plants zombies) = (GameModel (sun + length (filter (\((x,y), plant) -> isSunflower plant) plants) * 25) (cherryBombs plants) (zombieMapAlakitas plants zombies))

isSunflower :: Plant -> Bool
isSunflower (Sunflower _) = True
isSunflower _ = False

cherryBombs :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
cherryBombs [] = []
cherryBombs ((coord, plant):plants)
    | CherryBomb 0 <- plant = cherryBombs plants
    | CherryBomb _ <- plant = ((coord, plant):cherryBombs plants)
    | otherwise = ((coord, plant):cherryBombs plants)

zombieMapAlakitas :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zombieMapAlakitas [] zombies = zombies
zombieMapAlakitas ((coord, plant):plants) zombies
    | Peashooter _ <- plant = zombieMapAlakitas plants (peashooterAction coord zombies)
    | CherryBomb _ <- plant = zombieMapAlakitas plants (cherryBombAction coord zombies)
    | otherwise = zombieMapAlakitas plants zombies

peashooterAction :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
peashooterAction (x,y) zombies = map (\(coord, zombie) -> if (fst coord == x) && (snd coord >= y) then (coord, hitZombie zombie) else (coord, zombie)) zombies


hitZombie :: Zombie -> Zombie
hitZombie (Basic health speed) = Basic (health-1) speed
hitZombie (Conehead health speed) = Conehead (health-1) speed
hitZombie (Buckethead health speed) = Buckethead (health-1) speed
hitZombie (Vaulting health speed) = Vaulting (health-1) speed

cherryBombAction :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
cherryBombAction _ [] = []
cherryBombAction (x,y) ((coord, zombie):zombies)
    | x == fst coord, y == snd coord = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord, y == snd coord - 1 = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord, y == snd coord + 1 = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord - 1, y == snd coord = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord - 1, y == snd coord - 1 = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord - 1, y == snd coord + 1 = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord + 1, y == snd coord = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord + 1, y == snd coord - 1 = ((coord, zombie):cherryBombAction (x,y) zombies)
    | x == fst coord + 1, y == snd coord + 1 = ((coord, zombie):cherryBombAction (x,y) zombies)
    | otherwise = ((coord, zombie):cherryBombAction (x,y) zombies)



defendsAgainst :: GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainst (GameModel sun plants zombies) zombieList = defendsAgainst' (GameModel sun plants zombies) zombieList 2

defendsAgainst' :: GameModel -> [[(Int, Zombie)]] -> Int -> Bool
defendsAgainst' _ [] _ = True
defendsAgainst' (GameModel sun plants zombies) (zombie : zombieList) round
    | performZombieActions (GameModel sun plants zombies) == Nothing = False
    | (round == 2) = defendsAgainst' (performPlantActions (GameModel sun plants zombies)) (zombie:zombieList) (round + 1)
    | (round == 3) = defendsAgainst' (cleanBoard (GameModel sun plants zombies)) (zombie:zombieList) (round + 1)
    | (round == 4) = defendsAgainst' (fromMaybe (performZombieActions (GameModel sun plants zombies))) (zombie:zombieList) (round + 1)
    | (round == 5) = defendsAgainst' (GameModel sun plants (zombies ++ (map (\(x, zombie) -> ((x,11), zombie)) zombie))) zombieList (round + 1)
    | (round == 6) = defendsAgainst' (cleanBoard (GameModel sun plants zombies)) (zombie:zombieList) (round + 1)
    | (round == 7) = defendsAgainst' (GameModel (giveSun sun) plants zombies) (zombie:zombieList) 2

fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x

giveSun :: Int -> Int
giveSun sun = sun + 25

