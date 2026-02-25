module ScoreCalc where

import Types
-- The functions in this file calculates the score of a hand

-- Base chips and multiplier for each hand type (taken from Balatro)
handScore :: HandType -> (Int, Int)
handScore HighCard      = (5,   1)
handScore OnePair       = (10,  2)
handScore TwoPair       = (20,  2)
handScore ThreeOfAKind  = (30,  3)
handScore Straight      = (30,  4)
handScore Flush         = (35,  4)
handScore FullHouse     = (40,  4)
handScore FourOfAKind   = (60,  7)
handScore StraightFlush = (100, 8)
handScore RoyaleFlush   = (100, 8)

-- Chip value of each card rank (also taken form Balatro)
rankChips :: Rank -> Int
rankChips Two   = 2
rankChips Three = 3
rankChips Four  = 4
rankChips Five  = 5
rankChips Six   = 6
rankChips Seven = 7
rankChips Eight = 8
rankChips Nine  = 9
rankChips Ten   = 10
rankChips Jack  = 10
rankChips Queen = 10
rankChips King  = 10
rankChips Ace   = 11

-- calculate the score of a hand
calcScore :: HandType -> [Card] -> Int 
calcScore ht scoreCards = 
  let (base, mult) = handScore ht
      cardChips = sum (map(rankChips . rank) scoreCards)
  in (base + cardChips) * mult