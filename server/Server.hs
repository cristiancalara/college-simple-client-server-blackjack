import Network
import System.IO

import System.Random
import Data.Char

import DataTypes
import Deck
import BlackJack


-- Display the bank's final score and the winner.
displayOutcome :: Handle -> Hand -> Hand -> IO ()
displayOutcome handle deck guestHand = do
  putStrLn "Display outcome"
  hPutStrLn handle ("The bank's final score: " ++ show (value bank))
  hPutStrLn handle (show (winner guestHand bank))
  where
  bank = drawAllForBank deck Empty


displayBankInitialHand :: Handle -> Hand -> IO ()
displayBankInitialHand handle hand = do
  hPutStrLn handle (show(hand))

  -- Play until the guest player is bust or chooses to stop.
play ::  Handle -> Hand -> Hand -> IO ()
play handle deck guestHand = do
  hPutStrLn handle  ("Your cards: ")
  hPutStrLn handle  (show (guestHand))
  hPutStrLn handle  ("Your points: " ++ show (value guestHand))
  if over21 guestHand || equal21 guestHand then do
    hPutStrLn handle "gameOverTrue"
    displayOutcome handle deck guestHand
   else do
    hPutStrLn handle "gameOverFalse"
    hPutStrLn handle "Draw new card? (y/n)"
    answer <- hGetLine handle
    if null answer || not (map toLower answer == "n") then do
      putStrLn "Draws card"
      let (deck', guestHand') = draw deck guestHand
      play handle deck' guestHand'
     else
      displayOutcome handle deck guestHand

main :: IO ()
main = withSocketsDo $ do
         sock <- listenOn $ PortNumber 3001
         putStrLn "Starting server ..."
         handleConnections sock

handleConnections :: Socket -> IO ()
handleConnections sock = do
  (handle, host, port) <- accept sock
  hSetBuffering handle NoBuffering

  -- create the initial dealer card, and shuffle deck
  g <- newStdGen
  let (bankInitialCard, remainingDeck) = startPlay g
  displayBankInitialHand handle bankInitialCard

  -- start game loop
  g <- newStdGen
  play handle (remainingDeck) (Empty)
  handleConnections sock