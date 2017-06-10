module BlackJack where

import System.Random

import DataTypes
import Deck

-- returns the value of a card for Blackjach
-- rules 
    -- 2-10 represent their values
    -- Jack, Queen, King are 10
    -- Ace is initially 11, but can be changed to 1 if busted
valueRank :: Rank -> Integer
valueRank (Numeric x) = x
valueRank Ace = 11
valueRank _ = 10

-- value for a card
valueCard :: Card -> Integer
valueCard (Card rank _) = valueRank rank

-- returns the value for a hand WITHOUT aces condition
valueHand :: Hand -> Integer
valueHand Empty = 0
valueHand (Add card hand) = valueCard card + valueHand hand

-- helper function for the aces condition i.e. either 1 or 11
numberOfAces :: Hand -> Integer
numberOfAces Empty = 0
numberOfAces (Add (Card Ace suit) hand) = 1 + numberOfAces hand
numberOfAces (Add (Card _ suit) hand) = numberOfAces hand

-- returns the value for a hand WITH aces condition
value :: Hand -> Integer
value hand
  | valueHand hand > 21 = valueHand hand - (10 * numberOfAces hand)
  | otherwise = valueHand hand


-- draws a card from the deck, and puts it player hand
draw :: Hand -> Hand -> (Hand, Hand)
draw Empty _ = error ("Deck is empty")
draw (Add card rDeck) hand = (rDeck, Add card hand)


startPlay :: StdGen -> (Hand, Hand)
startPlay generator = ((Add (getCardAt theDeck index) Empty), (deleteCardAt theDeck index))
  where (index, _) = randomR(1, 52) generator
        theDeck = shuffleDeck generator (fullDeck)

-- bank needs to draw a new card if under 17, or stay as is after 17
drawAllForBank :: Hand -> Hand -> Hand
drawAllForBank deck bankHand  | value bankHand < 17 = drawAllForBank deck' bankHand'
                        | otherwise = bankHand
  where (deck' , bankHand') = draw deck bankHand  

-- if player has over 21, they are busted
over21 :: Hand -> Bool
over21 hand = value hand > 21

-- if player has = 21, we shouldn't ask him to draw
equal21 :: Hand -> Bool
equal21 hand = value hand == 21

winner :: Hand -> Hand -> Outcome
winner handGuest handBank 
  | over21(handGuest) = Winner Bank
  | over21(handBank) = Winner Guest
  | value(handGuest) > value(handBank) = Winner Guest
  | value(handBank) > value(handGuest) = Winner Bank
  | value(handGuest) == value(handBank) = Tie -- bank wins on draws