module Lesson where
import Data.Char

type ABC = [Char]
abc :: ABC
abc = ['A'..'Z']

type Riddle       = String
type RightGuesses = [Char]
type WrongGuesses = [Char]

data State = Riddle RightGuesses WrongGuesses
    deriving(Show, Eq)

isValidLetter :: Char -> String -> Bool
isValidLetter c s = toUpper c `elem` s

startState :: ABC -> String -> State
startState abc riddle = Riddle [] []
