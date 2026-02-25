module Types where

-- This file defines all the important types that are going to be used in the game

-- Card suits
data Suit = Clubs | Diamonds | Hearts | Spades
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- Card ranks from low to high
data Rank 
  = Two | Three | Four | Five | Six | Seven | Eight | Nine | Ten
  | Jack | Queen | King | Ace
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- Card
data Card = Card
  { rank :: Rank
  , suit :: Suit
  } 
  deriving (Eq, Show, Read)

-- Hand type ranked from worst to best
data HandType
  = HighCard
  | OnePair
  | TwoPair
  | ThreeOfAKind
  | Straight
  | Flush
  | FullHouse
  | FourOfAKind
  | StraightFlush
  | RoyaleFlush
  deriving (Eq, Ord, Enum, Bounded, Show, Read)

-- Hand result
data HandResult = HandResult
  { handType :: HandType
  , baseChips :: Int
  , multiplier :: Int
  }
  deriving (Eq, Show)

-- Current state of the game
data GameState = GameState
  { deck         :: [Card]
  , currentHand  :: [Card]
  , score        :: Int
  , targetScore  :: Int
  , handsLeft    :: Int
  , discardsLeft :: Int
  } 
  deriving (Show)
