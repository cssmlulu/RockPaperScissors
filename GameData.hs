module GameData (
    randomStrategy,
    convertStrategy,
    playGame ,
    printStats,
    updateStats,
    Stats(..),
    Strategy (..),
    Result (..)
    ) where

import Data.Char(toLower)
-- Strategy
data Strategy = Rock | Paper | Scissors
    deriving (Show, Eq)

instance Ord Strategy where
    compare x y 
        | x == Rock     && y == Scissors    = GT
        | x == Paper    && y == Rock        = GT
        | x == Scissors && y == Paper       = GT
        | x == y                            = EQ
        | otherwise                         = LT

convertStrategy :: String -> Maybe Strategy
convertStrategy str
    | str' == "rock" = Just Rock
    | str' == "paper" = Just Paper
    | str' == "scissors" = Just Scissors
    | length str/=1 = Nothing
    | x `elem` "Rr" = Just Rock
    | x `elem` "Pp" = Just Paper
    | x `elem` "Ss" = Just Scissors
    | otherwise = Nothing
    where
        str' = map toLower str
        x = toLower $ head str

randomStrategy :: Int -> Strategy
randomStrategy x
    | x == 0  = Rock
    | x == 1  = Paper
    | x == 2  = Scissors


playGame :: Strategy -> Strategy -> Result
playGame x y 
    | x == y    = Draw
    | x < y     = Win
    | otherwise = Lose


-- Result
data Result = Win | Lose | Draw
    deriving (Eq, Ord)

instance Show Result where
    show x = case x of
        Win  -> "You win!"
        Lose -> "You lose!"
        Draw  -> "Draw!" 



-- Status
type Stats = (Int, Int, Int)

updateStats :: Stats -> Result -> Stats
updateStats (w,l,d) rst = case rst of
    Win  -> (w+1, l,   d)
    Lose -> (w,   l+1, d)
    Draw  -> (w,   l,   d+1)

printStats :: Stats -> String
printStats (w,l,d) = let wins   = fromIntegral w
                         loses = fromIntegral l
                         draws   = fromIntegral d
                         rounds  = wins + loses + draws
                         winP   = wins / rounds
                         loseP  = loses / rounds
                         drawP   = draws / rounds
    in "Wins: " ++ show wins ++ " (" ++ show winP ++ "), Loses: " ++ show loses ++ 
            " (" ++ show loseP ++ "), Draws: " ++ show draws ++ " (" ++ show drawP ++ ")"