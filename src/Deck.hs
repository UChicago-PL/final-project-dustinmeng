module Deck where

import Types
import System.Random (RandomGen, uniformR)

-- Deck related functions including generating deck, randomizing deck and dealing cards

-- Generate a full deck (52 cards without the jokers)
fullDeck :: [Card]
fullDeck = [Card r s | s <- [minBound..maxBound], r <- [minBound..maxBound]]

-- Shuffle pending


-- Deal n cards from the top of the deck returns a tuple with dealt and remaining cards
deal :: Int -> [Card] -> ([Card], [Card])
deal n d = splitAt n d