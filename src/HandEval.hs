module HandEval where

import Types
import Data.List (sort, group, sortBy, maximumBy)



-- Evaluates the 5 card hand, returns the handtype and the cards contribute to the handtype
eval :: [Card] -> (HandType, [Card])
eval cards
  | isRoyaleFlush   = (RoyaleFlush,   cards)
  | isStraightFlush = (StraightFlush, cards)
  | isFourOfAKind   = (FourOfAKind,   getScoringCards 4)
  | isFullHouse     = (FullHouse,     cards)
  | isFlush         = (Flush,         cards)
  | isStraight      = (Straight,      cards)
  | isThreeOfAKind  = (ThreeOfAKind,  getScoringCards 3)
  | isTwoPair       = (TwoPair,       getScoringCards 2)
  | isOnePair       = (OnePair,       getScoringCards 2)
  | otherwise       = (HighCard,      [highestCard])
  where
    ranks = map rank cards
    suits = map suit cards
    sorted = sort ranks
    grouped = group sorted
    counts = map length grouped
    groups = sortBy (flip compare) counts

    isFlush = all (== head suits) (tail suits)
    isStraight = isConsecutive sorted || sorted == [Two, Three, Four, Five, Ace]
    isStraightFlush = isFlush && isStraight
    isRoyaleFlush = isFlush && sorted == [Ten, Jack, Queen, King, Ace]
    isFourOfAKind = groups == [4, 1]
    isFullHouse = groups == [3, 2]
    isThreeOfAKind = groups == [3, 1, 1]
    isTwoPair = groups == [2, 2, 1]
    isOnePair = groups == [2, 1, 1, 1]

    -- Get cards whose rank appears exactly n times
    getScoringCards n =
      let scoringRanks = [head g | g <- grouped, length g == n]
      in filter (\c -> rank c `elem` scoringRanks) cards

    -- Highest card
    highestCard = maximumBy (\a b -> compare (rank a) (rank b)) cards

-- check if a sorted list of ranks is consecutive
isConsecutive :: [Rank] -> Bool
isConsecutive [] = True
isConsecutive [_] = True
isConsecutive (a:b:rest) = 
  succ a == b && isConsecutive (b:rest)