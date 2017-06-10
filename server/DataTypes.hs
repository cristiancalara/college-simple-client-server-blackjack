module DataTypes where

-- The suit
data Suit = Hearts | Spades | Diamonds | Clubs deriving (Eq, Show, Read)

-- The rank of cards. 2-10 = Numeric 2-10, and the others
data Rank = Numeric Integer | Jack | Queen | King | Ace deriving (Eq, Show, Read)

-- The card type
data Card = Card { rank :: Rank, suit :: Suit } deriving (Eq, Show, Read)

-- A hand of cards. More like a collection as is used to represent a deck
data Hand = Empty | Add Card Hand deriving (Eq, Show, Read)

-- The player can be either a guest or bank
data Player = Guest | Bank deriving (Show, Eq, Read)

-- The player can be either a guest or bank
data Outcome = Winner Player | Tie deriving (Show, Eq, Read)
