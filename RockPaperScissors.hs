module RockPaperScissors (
    playGame
    ) where

import Data


playGame :: Strategy -> Strategy -> Result
playGame x y 
    | x == y    = Draw
    | x < y     = Win
    | otherwise = Lose