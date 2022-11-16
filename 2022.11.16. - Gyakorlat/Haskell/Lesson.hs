module Lesson where

data Privilege = Unprivileged | Admin
    deriving(Show, Eq)

data Cookie = LoggedIn String Privilege | LoggedOut
    deriving(Show, Eq)

type DB = [(String,String,Privilege)]

db :: DB
db = [("dumbledore","abracadabra",Unprivileged), ("root", "secret", Admin), ("bela", "korte", Unprivileged)]

register :: String -> String -> Cookie -> DB -> DB
register username password (LoggedIn _ Admin) l = (username, password, Unprivileged) : db
register _ _ _ l = l

-- data Maybe a = Just a | Nothing

safeDiv :: Int -> Int -> Maybe Int
safeDiv _ 0 = Nothing
safeDiv a b = Just (div a b)

getUser :: String -> DB -> Maybe (String, Privilege)
getUser username [] = Nothing
getUser username ((u,p,pr):xs)
    | username == u = Just (p,pr) 
    | otherwise = getUser username xs

login :: String -> String -> DB -> Cookie
login username passw [] = LoggedOut
login username passw ((u,p,pr):xs)
    | username == u && passw == p = LoggedIn u pr
    | otherwise = login username passw xs

passwd :: String -> Cookie -> DB -> DB
passwd _ _ [] = []
passwd new (LoggedIn user pr1) l@((u,p,pr):xs) 
    | user == u = drop 1 l ++ [(u,new,pr)]
    | otherwise = (u,p,pr) : passwd new (LoggedIn user pr1) xs
