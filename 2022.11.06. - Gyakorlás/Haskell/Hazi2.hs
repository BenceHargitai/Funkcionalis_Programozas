module Lesson where


osszes :: [Char]
osszes = ['0'..'9'] ++ ['A'..'Z'] ++ ['a'..'z']

mapping :: [(Char,Char)]
mapping  = zip osszes (drop 3 [x | x <- osszes] ++ take 3 [x | x <- osszes])

encodeCaesar :: String -> String
encodeCaesar = map (\x -> 
    if length (filter (\(a,b) -> a == x) mapping) > 0 then 
        snd (filter (\(a,b) -> a == x) mapping !! 0) 
    else 
        '?')


decodeCaesar ::String -> String
decodeCaesar = map (\x -> 
    if length (filter (\(a,b) -> b == x) mapping) > 0 then 
        fst (filter (\(a,b) -> b == x) mapping !! 0) 
    else 
        '?') 


