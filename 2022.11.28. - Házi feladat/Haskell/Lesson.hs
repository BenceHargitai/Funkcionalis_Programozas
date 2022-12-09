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

--Valamiért kiírja a két intet is nálam
placeZombieInLane :: GameModel -> Zombie -> Int -> Maybe GameModel
placeZombieInLane (GameModel sun plants zombies) zombie lane
    | lane >= 5 || lane < 0 = Nothing 
    | (lane, 11) `elem` (map fst zombies) = Nothing
    | otherwise = Just (GameModel sun plants (((lane,11),zombie):zombies))

-- =============================================================================
--                              performZombieActions
-- =============================================================================

performZombieActions :: GameModel -> Maybe GameModel
performZombieActions (GameModel sun plants zombies)
    | (any (\((x,y),zombie) -> y == 0) zombies) = Nothing
    | otherwise = Just (GameModel sun (hitPlants zombies plants) (changeZombiePosition plants zombies))


hitPlants :: [(Coordinate, Zombie)] -> [(Coordinate, Plant)] -> [(Coordinate, Plant)]
hitPlants [] plants = plants
hitPlants ((coord, zombie):zombies) plants
    | Basic _ _ <- zombie = hitPlants zombies (hitPlantAt coord plants) 
    | Conehead _ _ <- zombie = hitPlants zombies (hitPlantAt coord plants)
    | Buckethead _ _ <- zombie = hitPlants zombies (hitPlantAt coord plants)
    | Vaulting _ 1 <- zombie = hitPlants zombies (hitPlantAt coord plants)
    | Vaulting _ 2 <- zombie = hitPlants zombies plants

hitPlantAt :: Coordinate -> [(Coordinate, Plant)] -> [(Coordinate, Plant)]
hitPlantAt coord [] = []
hitPlantAt coord ((coord2, plant):plants)
    | coord == coord2 = ((coord2, meleePlant plant):hitPlantAt coord plants)
    | otherwise = ((coord2, plant):hitPlantAt coord plants)

meleePlant :: Plant -> Plant
meleePlant (Peashooter health) = Peashooter (health-1)
meleePlant (Sunflower health) = Sunflower (health-1)
meleePlant (Walnut health) = Walnut (health-1)
meleePlant (CherryBomb health) = CherryBomb (health-1)


changeZombiePosition :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
changeZombiePosition plants zombies = map (\(coord, zombie) -> 
    -- *Ha a zombie vaultingos
    if (isVaulting zombie) 
        then 
        -- *Ha a  mezőn van növény, akkor ugorja át, és csökkentse le a speedjét
            if plantHere (fst coord, snd coord) plants 
                then ((jumpShort (fst coord, snd coord)), Vaulting (getZombieHealth zombie) 1)
        -- *Ha az előtte lévő mezőn van növény, akkor ugorj a át, és csökkentse le a speedjét
            else if plantHere (fst coord, snd coord-1) plants 
                then ((jumpLong (fst coord, snd coord)), Vaulting (getZombieHealth zombie) 1)
        -- *Ha a mezőn nincs növény
            else 
                (jumpLong(fst coord, snd coord), zombie)
    else if plantHere coord plants
        then (coord, zombie) 
    else ((fst coord, snd coord - 1), zombie)) zombies

isVaulting :: Zombie -> Bool
isVaulting (Vaulting _ 2) = True
isVaulting _ = False

jumpShort :: Coordinate -> Coordinate
jumpShort (x,y) 
    | y-1 <= 0 = (x,0)
    | otherwise = (x,y-1)

jumpLong :: Coordinate -> Coordinate
jumpLong (x,y) 
    | y-2 <= 0 = (x,0)
    | otherwise = (x,y-2)


getZombieHealth :: Zombie -> Int
getZombieHealth (Vaulting health _) = health

plantHere :: Coordinate -> [(Coordinate, Plant)] -> Bool
plantHere coord [] = False
plantHere coord ((coord2, plant):plants)
    | coord == coord2 = True
    | otherwise = plantHere coord plants

-- =============================================================================
--                              cleanBoard
-- =============================================================================

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
    | Basic health _ <- zombie, health <= 0 = removeZombie zombies
    | Conehead health _ <- zombie, health <= 0 = removeZombie zombies
    | Buckethead health _ <- zombie, health <= 0 = removeZombie zombies
    | Vaulting health _ <- zombie, health <= 0 = removeZombie zombies
    | otherwise = ((coord, zombie):removeZombie zombies)


-- =============================================================================
--                              performPlantActions
-- =============================================================================

performPlantActions :: GameModel -> GameModel
performPlantActions (GameModel sun plants zombies) = (GameModel (sun + length (filter (\((x,y), plant) -> isSunflower plant) plants) * 25) (cherryBombs plants) (zombieThings plants zombies))

isSunflower :: Plant -> Bool
isSunflower (Sunflower _) = True
isSunflower _ = False

cherryBombs :: [(Coordinate, Plant)] -> [(Coordinate, Plant)]
cherryBombs ((cord, CherryBomb x) : xs) = (cord, CherryBomb 0) : cherryBombs xs
cherryBombs (x:xs) = x : cherryBombs xs
cherryBombs [] = []

zombieThings :: [(Coordinate, Plant)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
zombieThings [] zombies = zombies
zombieThings ((coord, plant):plants) zombies
    | Peashooter _ <- plant = zombieThings plants (shootPea (firstZombie coord zombies) zombies)
    | CherryBomb _ <- plant = zombieThings plants (explode coord zombies)
    | otherwise = zombieThings plants zombies


firstZombie :: Coordinate -> [(Coordinate, Zombie)] -> Coordinate
firstZombie (x,y) zombies 
    | length (filter (\(coord, zombie) -> (fst coord == x) && (snd coord >= y)) zombies) == 0 = (-1,-1)
    | otherwise = fst (head (filter (\(coord, zombie) -> (fst coord == x) && (snd coord >= y)) zombies))

-- shootPea :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
-- shootPea (x,y) zombies
--     | x == -1 &&  y == -1 = zombies
--     | otherwise = map (\(coord, zombie) -> if coord == (x,y) then (coord, hitZombie zombie) else (coord, zombie)) zombies

shootPea :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
shootPea (-1,-1) zombies = zombies
shootPea coord [] = []
shootPea coord ((coord2, zombie):zombies)
    | coord == coord2 = ((coord2, hitZombie zombie):shootPea coord zombies)
    | otherwise = ((coord2, zombie):shootPea coord zombies)




hitZombie :: Zombie -> Zombie
hitZombie (Basic health speed) = Basic (health-1) speed
hitZombie (Conehead health speed) = Conehead (health-1) speed
hitZombie (Buckethead health speed) = Buckethead (health-1) speed
hitZombie (Vaulting health speed) = Vaulting (health-1) speed

explode :: Coordinate -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
explode (x,y) zombies = map (\(coord, zombie) -> if (fst coord >= x-1) && (fst coord <= x+1) && (snd coord >= y-1) && (snd coord <= y+1) then (coord, destroyZombie zombie) else (coord, zombie)) zombies

destroyZombie :: Zombie -> Zombie
destroyZombie (Basic _ speed) = Basic 0 speed
destroyZombie (Conehead _ speed) = Conehead 0 speed
destroyZombie (Buckethead _ speed) = Buckethead 0 speed
destroyZombie (Vaulting _ speed) = Vaulting 0 speed

-- =============================================================================
--                              defendsAgainst
-- =============================================================================


defendsAgainst :: GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainst (GameModel sun plants zombies) zombieList = defendsAgainst' (GameModel sun plants zombies) zombieList 2


defendsAgainst' :: GameModel -> [[(Int, Zombie)]] -> Int -> Bool
defendsAgainst' (GameModel sun plants zombies) loadZombies round 
    --------------------------------     *defendsAgainstI    --------------------------------
    -- | (round == 1) = defendsAgainst' (defendsAgainstI (GameModel sun plants zombies) loadZombies) loadZombies 2

    --------------------------------     *Performplantactions    --------------------------------
    | (round == 2) = defendsAgainst' (performPlantActions (GameModel sun plants zombies)) loadZombies 3 

    --------------------------------        *cleanboard          --------------------------------
    | (round == 3) = defendsAgainst' (cleanBoard (GameModel sun plants zombies)) loadZombies 4 

    --------------------------------     *PerformZombieActions   --------------------------------
    | (round == 4) = defendsAgainst' (fromMaybe (performZombieActions (GameModel sun plants zombies))) loadZombies 5
        
     --------------------------------     *PerformZombieActions   --------------------------------
    | (round == 5) = 
        if (performZombieActions (GameModel sun plants zombies) == Nothing) 
            then False 
        else defendsAgainst' (GameModel sun plants zombies) loadZombies 6
        
    --------------------------------      *zombik feltöltése     --------------------------------
    | (round == 6) =
        if (loadZombies == [])
            then
            defendsAgainst' (GameModel sun plants zombies) loadZombies 7
        else
            defendsAgainst' (GameModel sun plants (placeZombiesfromlist (head loadZombies) zombies)) (removeHead loadZombies) 7 

    --------------------------------         *cleanboard          --------------------------------
    | (round == 7) = defendsAgainst' (cleanBoard (GameModel sun plants zombies)) loadZombies 8

    ----------  *megnézzük, hogy üres-e mindkét zombi lista, ha igen akkor True  -----------------
    | (round == 8) =
        if (loadZombies == [] && zombies == [])
            then True
        else
            defendsAgainst' (GameModel sun plants zombies) loadZombies 9
            
    --------------------------------       *nap növelése          --------------------------------
    | (round == 9) = defendsAgainst' (GameModel (sun+25) plants zombies) loadZombies 2


fromMaybe :: Maybe a -> a
fromMaybe (Just x) = x


placeZombiesfromlist :: [(Int, Zombie)] -> [(Coordinate, Zombie)] -> [(Coordinate, Zombie)]
placeZombiesfromlist [] zombies = zombies
placeZombiesfromlist (x:xs) defaultZombies = placeZombiesfromlist xs (defaultZombies ++ ((fst x, 11), snd x):[])

removeHead :: [[a]] -> [[a]]
removeHead [] = []
removeHead (x:xs) = xs


-- =============================================================================
--                              defendsAgainstI
-- =============================================================================

defendsAgainstI :: (GameModel -> GameModel) -> GameModel -> [[(Int, Zombie)]] -> Bool
defendsAgainstI f (GameModel sun plants zombies) loadZombies = defendsAgainst' (f (GameModel sun plants zombies)) loadZombies 2

