{-# LANGUAGE GADTs #-}

module Card (module Card) where

import Model

pool :: [Card]
pool = [dummy, dumber, triDummy, dumbo, bigDumbo, kingDumbo]

defCard :: Card
defCard = Card {_cardName = PlaceHolder, _cardTier = -1, _baseCost = -1, _attack = -1, _health = -1, _deathrattle = []}

skeleton :: Card
skeleton = defCard {_cardName = Skeleton, _cardTier = 1, _baseCost = error "Skeleton has no base cost.", _attack = 1, _health = 1}

harmlessBonehead :: Card
harmlessBonehead =
  defCard
    { _cardName = HarmlessBonehead,
      _cardTier = 1,
      _baseCost = 3,
      _attack = 1,
      _health = 1,
      _deathrattle = [Summon (SpecificCard skeleton), Summon (SpecificCard skeleton)]
    }

dummy :: Card
dummy = defCard {_cardName = Dummy, _cardTier = 1, _baseCost = 3, _attack = 1, _health = 1}

dumber :: Card
dumber = defCard {_cardName = Dumber, _cardTier = 2, _baseCost = 3, _attack = 2, _health = 2}

triDummy :: Card
triDummy = defCard {_cardName = TriDummy, _cardTier = 3, _baseCost = 3, _attack = 3, _health = 3}

dumbo :: Card
dumbo = defCard {_cardName = Dumbo, _cardTier = 4, _baseCost = 3, _attack = 4, _health = 4}

bigDumbo :: Card
bigDumbo = defCard {_cardName = BigDumbo, _cardTier = 5, _baseCost = 3, _attack = 5, _health = 5}

kingDumbo :: Card
kingDumbo = defCard {_cardName = KingDumbo, _cardTier = 6, _baseCost = 3, _attack = 6, _health = 6}

dummyWithALongNameItKeepsGoing :: Card
dummyWithALongNameItKeepsGoing = defCard {_cardName = DummyWithALongNameItKeepsGoing, _cardTier = 1, _baseCost = 3, _attack = 1, _health = 1}
