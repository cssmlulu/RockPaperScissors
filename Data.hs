module Data (
    randomStrategy,
    convertStrategy,
    Strategy (..),
    Result (..)
    ) where

import System.Random

data Strategy = Rock | Paper | Scissors
    deriving (Show, Eq)

instance Ord Strategy where
    compare x y 
        | x == Rock     && y == Scissors    = GT
        | x == Paper    && y == Rock        = GT
        | x == Scissors && y == Paper       = GT
        | otherwise                         = LT

convertStrategy :: Char -> Maybe Strategy
convertStrategy x
    | x `elem` "Rr" = Just Rock
    | x `elem` "Pp" = Just Paper
    | x `elem` "Ss" = Just Scissors
    | otherwise = Nothing

randomStrategy :: Int -> Strategy
randomStrategy x
    | x == 0  = Rock
    | x == 1  = Paper
    | x == 2  = Scissors


data Result = Win | Lose | Draw
    deriving (Eq, Ord)

instance Show Result where
    show x = case x of
        Win  -> "You win!"
        Lose -> "You lose!"
        Draw  -> "Draw!" 