module Main (main) where

import Data
import RockPaperScissors (playGame)
import System.IO
import Control.Applicative
import System.Random


main :: IO ()
main = do
    -- player
    putStrLn "Enter r for Rock, p for Paper or s for Scissors\n"
    putStr "Your choice: "
    hFlush stdout
    playerChoose <- getChar
    let player   = convertStrategy playerChoose

    -- computer
    randNum <- getStdRandom (randomR (0,2))
    putStr "Computer's choice: "
    hFlush stdout
    let computer = randomStrategy randNum
    putStrLn $ show computer

    -- result
    let result = playGame computer <$> player
    case result of
        Nothing -> do
            putStrLn "Invalid move."
        Just x  -> do
            putStrLn $ show x   