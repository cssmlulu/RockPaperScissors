module Data (
    Strategy (..),
    Result (..)
    ) where


data Strategy = Rock | Paper | Scissors
    deriving (Show, Eq)

instance Ord Strategy where
    compare x y 
        | x == Rock     && y == Scissors    = GT
        | x == Paper    && y == Rock        = GT
        | x == Scissors && y == Paper       = GT
        | otherwise                         = LT


data Result = Win | Lose | Draw
    deriving (Eq, Ord)

instance Show Result where
    show x = case x of
        Win  -> "You win!"
        Lose -> "You lose!"
        Draw  -> "Draw!" 