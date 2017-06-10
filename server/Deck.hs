module Deck where

import System.Random

import DataTypes

-- used to merge the two hands
mergeHands :: Hand -> Hand -> Hand
mergeHands Empty hand2 = hand2
mergeHands (Add card hand1) hand2 = Add card (mergeHands hand1 hand2)


allCardsForSuit :: Suit -> Hand
allCardsForSuit s = (Add (Card Ace s) (Add (Card King s) (Add (Card Queen s) (Add (Card Jack s) (Add (Card (Numeric 10) s) (Add (Card (Numeric 9) s) (Add (Card (Numeric 8) s) (Add (Card (Numeric 7) s) (Add (Card (Numeric 6) s) (Add (Card (Numeric 5) s) (Add (Card (Numeric 4) s) (Add (Card (Numeric 3) s) (Add (Card (Numeric 2) s)  Empty)))))))))))))

-- the full deck
fullDeck :: Hand
fullDeck = mergeHands (allCardsForSuit Spades) (mergeHands (allCardsForSuit Hearts) (mergeHands (allCardsForSuit Clubs) (allCardsForSuit Diamonds)))


handSize :: Num a => Hand -> a
handSize Empty            = 0
handSize (Add card hand)  = 1 + handSize hand

deleteCardAt :: Hand -> Integer -> Hand
deleteCardAt Empty _ = Empty
deleteCardAt (Add card hand) index
  | index == 1 = hand
  | otherwise = (Add card (deleteCardAt hand (index-1)))


getCardAt :: Hand -> Integer -> Card
getCardAt Empty _ = error ("Hand is empty") 
getCardAt (Add card hand) index
  | index == 1 = card
  | otherwise = getCardAt hand (index-1) 


-- shuffles the full deck
-- as we only have pure functions, we need to provide a StdGen (using newStdGen) for a 'random' value
shuffleDeck :: StdGen -> Hand -> Hand
shuffleDeck _ Empty = Empty 
shuffleDeck startRand deck = (Add (getCardAt deck randIndex)(shuffleDeck newStdGen (deleteCardAt deck randIndex)))
  where (randIndex, newStdGen) = randomR (1, handSize deck) startRand