module Main (main) where

import RockPaperScissors (playGame)
import System.IO
import Control.Applicative
import Data 
import System.Random


main :: IO ()
main = do
    putStrLn "Enter r for Rock, p for Paper or s for Scissors\n"
    putStr "Your choice: "
    hFlush stdout
    playerChoose <- getChar
    let player   = convertStrategy playerChoose
    randNum <- getStdRandom (randomR (0,2))
    let computer = randomStrategy randNum
    
    let result = playGame computer <$> player
    case result of
        Nothing -> do
            putStrLn "Invalid move."
        Just x  -> do
            putStrLn $ show x   