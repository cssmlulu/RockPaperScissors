module Main (main) where

import RockPaperScissors (playGame)
import System.IO
import Control.Applicative
import Data 

main :: IO ()
main = do
    putStr "\nYou choose: "
    hFlush stdout
    playerChoose <- getChar
    let player   = convertStrategy playerChoose
    let computer = Rock
    
    let result = playGame computer <$> player
    case result of
        Nothing -> do
            putStrLn "Invalid move."
        Just x  -> do
            putStrLn $ show x   