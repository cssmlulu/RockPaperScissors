module RockPaperScissors (
    setChoice,
    playGame
    ) where


import Data.Char (toLower)
import System.IO
import Data

setChoice :: String -> Maybe Strategy
setChoice x = case map toLower x of
    "rock"     -> Just Rock
    "paper"    -> Just Paper
    "scissors" -> Just Scissors
    otherwise  -> Nothing

playGame :: Strategy -> Strategy -> Result
playGame x y 
    | x == y    = Draw
    | x < y     = Win
    | otherwise = Lose