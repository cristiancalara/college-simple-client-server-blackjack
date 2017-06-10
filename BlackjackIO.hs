module BlackJackIO where

import System.Random
import Data.Char

import DataTypes
import Deck
import BlackJack


-- Display the bank's final score and the winner.
displayOutcome :: Hand -> Hand -> IO ()
displayOutcome deck guest = do
  putStrLn ("The bank's final score: " ++ show (value bank))
  putStrLn (show (winner guest bank))
  where
  bank = playBank deck


-- Play until the guest player is bust or chooses to stop.
gameLoop ::  Hand -> Hand -> IO ()
gameLoop deck guest = do
  putStrLn ("Your current score: " ++ show (value guest))
  if gameOver guest then do
    displayOutcome deck guest
   else do
    putStrLn "Draw another card? [y]"
    yn <- getLine
    if null yn || not (map toLower yn == "n") then do
      let (deck', guest') = draw deck guest
      gameLoop deck' guest'
     else
      displayOutcome deck guest

main :: IO ()
main = do
  putStrLn "Welcome to the game."
  g <- newStdGen
  gameLoop (shuffleDeck g (fullDeck)) (Empty)