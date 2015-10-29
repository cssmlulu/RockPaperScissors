module Main (main) where

import RockPaperScissors (setChoice, playGame)
import System.IO
import Control.Applicative
import Data.Maybe
import Data

main :: IO ()
main = do
    putStr "\nYou choose: "
    hFlush stdout
    playerChoose <- getLine
    let player   = setChoice playerChoose
    let computer = Rock
    
    let result = playGame computer <$> player
    case result of
        Nothing -> do
            putStrLn "Invalid move."
        Just x  -> do
            putStrLn $ show x   