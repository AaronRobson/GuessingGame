module GuessingGame where

import System.Random
import Control.Monad
import Data.Char
import Text.Read

minNum :: Integer
minNum = 1

maxNum :: Integer
maxNum = 10

goesAllowed :: Integer
goesAllowed = 4

introduction :: String
introduction = "Guessing Game:"

instructions :: Integer -> Integer -> String
instructions from to = "Please enter a value between " ++ (show from) ++ " and " ++ (show to) ++ "."

randomNumber :: Integer -> Integer -> IO Integer
randomNumber a b = randomRIO (a,b) :: IO Integer

askForGuess :: IO (Maybe Integer)
askForGuess = do
    line <- getLine
    return $ readMaybe line

askForGuessUntilValid :: IO Integer
askForGuessUntilValid = do
    g <- askForGuess
    case g of 
      Just n -> return n
      Nothing -> do
        putStrLn "Please enter a whole number:"
        askForGuessUntilValid

playGame :: IO ()
playGame = do
    putStrLn $ instructions minNum maxNum
    r <- randomNumber minNum maxNum
    haveGuess goesAllowed r
  where
    haveGuess :: Integer -> Integer -> IO ()
    haveGuess goesLeft n = 
      if 0 < goesLeft
        then do putStrLn $ "You have " ++ (show goesLeft) ++ " goes remaining."
                guess <- askForGuessUntilValid
                if guess == n
                  then putStrLn "Well done."
                  else
                    if guess < n
                      then putStrLn "Too low." >> guessAgain
                      else putStrLn "Too high." >> guessAgain
        else putStrLn $ "You have ran out of goes. The number was: " ++ (show n) ++ "."
      where
        guessAgain :: IO ()
        guessAgain = haveGuess (pred goesLeft) n

responseIsPositive :: String -> Bool
responseIsPositive = (`elem` (map (map toLower) ["y", "yes","true","confirm", "1"])) . (map toLower)

askPlayAgain :: IO Bool
askPlayAgain = do
    putStrLn "Thanks for playing, would you like to go again?"
    response <- getLine
    return $ responseIsPositive response

playGames :: IO ()
playGames = do
    putStrLn introduction
    playGames'
  where
    playGames' :: IO ()
    playGames' = do
      putStrLn ""
      playGame
      putStrLn ""
      b <- askPlayAgain
      when b playGames'   

main :: IO ()
main = playGames
