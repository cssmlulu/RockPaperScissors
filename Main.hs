module Main (main) where

import RockPaperScissors

import System.IO
import Control.Applicative
import System.Random
import Data.Char(toLower)

--main :: IO Stats
main = do
    finalStat <- game (0,0,0)
    putStrLn "\n**************************\tGame over\t**********************"
    putStrLn $ "Result:\n" ++ printStats finalStat


game :: Stats -> IO Stats
game stats = do
    putStrLn "\n**************************\tNew Round\t**********************"
    putStrLn $ printStats stats
    -- player
    putStrLn "Enter r for Rock, p for Paper or s for Scissors"
    putStr "Your choice: "
    hFlush stdout
    playerChoose <- getLine
    let player   = convertStrategy $ toLower $ head playerChoose

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
            gameContinue stats
        Just x  -> do
            putStrLn $ show x
            let newStats = updateStats stats x
            gameContinue newStats

    

gameContinue :: Stats -> IO Stats
gameContinue stats = do
    putStr $ "Continue? [y/n]: "
    hFlush stdout
    continue <- getLine
    case map toLower continue of
        "n" -> return stats
        "y" -> game stats
        otherwise -> gameContinue stats
