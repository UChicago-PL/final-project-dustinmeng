module HandEval where

import Types
import Data.List (sort, group, sortBy)



-- Evaluates the 5 card hand
eval :: [Card] -> HandType
eval cards
  | isRoyaleFlush    = RoyaleFlush
  | isStraightFlush = StraightFlush
  | isFourOfAKind   = FourOfAKind
  | isFullHouse     = FullHouse
  | isFlush         = Flush
  | isStraight      = Straight
  | isThreeOfAKind  = ThreeOfAKind
  | isTwoPair       = TwoPair
  | isOnePair       = OnePair
  | otherwise       = HighCard
  where 
    ranks = map rank cards 
    suits = map suit cards 
    sorted = sort ranks 
    grouped = group sorted 
    counts = map length grouped 
    groups = sortBy (flip compare) counts
    -- checks if the hand is flush
    isFlush = all (== head suits) (tail suits)
    isStraight = isConsecutive sorted || sorted == [Two, Three, Four, Five, Ace]
    isStraightFlush = isFlush && isStraight
    isRoyaleFlush = isFlush && sorted == [Ten, Jack, Queen, King, Ace]
    isFourOfAKind = groups == [4,1]
    isFullHouse    = groups == [3, 2]
    isThreeOfAKind = groups == [3, 1, 1]
    isTwoPair      = groups == [2, 2, 1]
    isOnePair      = groups == [2, 1, 1, 1]

-- check if a sorted list of ranks is consecutive
isConsecutive :: [Rank] -> Bool
isConsecutive [] = True
isConsecutive [_] = True
isConsecutive (a:b:rest) = 
  succ a == b && isConsecutive (b:rest)