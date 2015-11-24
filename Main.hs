module Main (main) where

import RockPaperScissors

import System.IO
import Control.Applicative
import System.Random
import Data.Char(toLower)

main :: IO ()
main = do
    game


game :: IO ()
game = do
    -- player
    putStrLn "Enter r for Rock, p for Paper or s for Scissors\n"
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
        Just x  -> do
            putStrLn $ show x   
    -- ask whether continue game or not
    gameContinue

gameContinue :: IO ()
gameContinue = do
    putStr $ "Continue? [y/n]: "
    hFlush stdout
    continue <- getLine
    case map toLower continue of
        "n" -> return ()
        "y" -> game
        otherwise -> gameContinue
