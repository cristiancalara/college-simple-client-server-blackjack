import Network
import System.IO

import Data.Char
import DataTypes

welcomeMessage :: IO ()
welcomeMessage =  do
  putStrLn "---------------------------------------------"
  putStrLn "Blackjack"
  putStrLn "In order to win, you need to have more points"
  putStrLn "than the dealer but less than 21."
  putStrLn "Type y/n to play."
  putStrLn "----------------------------------------------"


-- Display the bank's final score and the winner.
displayOutcomeClient :: Handle -> IO ()
displayOutcomeClient handle = do
  putStrLn "----------------------------------------------"
  output <- hGetLine handle
  putStrLn output
  output <- hGetLine handle
  putStrLn output
  putStrLn "----------------------------------------------"


-- display the initial card dealt to the dealer
-- in a normal game, a card is flipped (such that the guest can see it)
displayBankInitialHandClient :: Handle -> IO ()
displayBankInitialHandClient handle  = do
  putStrLn "Dealer initial card:"
  output <- hGetLine handle
  putStrLn (userFriendlyHand (read output))


  -- Play until the guest player is bust or chooses to stop.
playClient ::  Handle -> IO ()
playClient handle = do
  putStrLn "---------------------------"
  output <- hGetLine handle
  putStrLn output
  output <- hGetLine handle -- my cards
  putStrLn (userFriendlyHand (read output))
  output <- hGetLine handle -- my points
  putStrLn output
  gameOverResp <- hGetLine handle -- get gane status
  if gameOverResp == "gameOverTrue" then do
    displayOutcomeClient handle
   else do
    output <- hGetLine handle
    putStrLn output
    answer <- getLine
    hPutStrLn handle answer
    if null answer || not (map toLower answer == "n") then do
      playClient handle
     else
      displayOutcomeClient handle


-- displays the user hand in a more friendly way
userFriendlyHand :: Hand -> String
userFriendlyHand Empty = ""
userFriendlyHand (Add (Card (Numeric i) suit) hand) = show(i) ++ " " ++ show(suit) ++ " " ++ userFriendlyHand hand
userFriendlyHand (Add (Card rank suit) hand) = show(rank) ++ " " ++ show(suit) ++ " " ++ userFriendlyHand hand

main :: IO ()
main = withSocketsDo $ do
         handle <- connectTo "localhost" (PortNumber 3001)
         hSetBuffering handle NoBuffering
         -- initial message
         welcomeMessage

         -- dealear's initial hand
         displayBankInitialHandClient handle

         -- play loop
         playClient handle
         hClose handle