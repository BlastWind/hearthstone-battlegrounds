{-# LANGUAGE GADTs #-}

module Card (module Card) where

import Model

pool :: [Card]
pool = [dummy, dumber, triDummy, dumbo, bigDumbo, kingDumbo]

defCard :: Card
defCard = Card {cardName = PlaceHolder, cardTier = -1, baseCost = -1, attack = -1, health = -1, deathrattle = []}

skeleton :: Card
skeleton = defCard {cardName = Skeleton, cardTier = 1, baseCost = error "Skeleton has no base cost.", attack = 1, health = 1}

harmlessBonehead :: Card
harmlessBonehead =
  defCard
    { cardName = HarmlessBonehead,
      cardTier = 1,
      baseCost = 3,
      attack = 1,
      health = 1,
      deathrattle = [Summon [skeleton, skeleton]]
    }

dummy :: Card
dummy = defCard {cardName = Dummy, cardTier = 1, baseCost = 3, attack = 1, health = 1}

dumber :: Card
dumber = defCard {cardName = Dumber, cardTier = 2, baseCost = 3, attack = 2, health = 2}

triDummy :: Card
triDummy = defCard {cardName = TriDummy, cardTier = 3, baseCost = 3, attack = 3, health = 3}

dumbo :: Card
dumbo = defCard {cardName = Dumbo, cardTier = 4, baseCost = 3, attack = 4, health = 4}

bigDumbo :: Card
bigDumbo = defCard {cardName = BigDumbo, cardTier = 5, baseCost = 3, attack = 5, health = 5}

kingDumbo :: Card
kingDumbo = defCard {cardName = KingDumbo, cardTier = 6, baseCost = 3, attack = 6, health = 6}

dummyWithALongNameItKeepsGoing :: Card
dummyWithALongNameItKeepsGoing = defCard {cardName = DummyWithALongNameItKeepsGoing, cardTier = 1, baseCost = 3, attack = 1, health = 1}
