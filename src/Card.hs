module Card (module Card) where

import Model


allCards :: [Card]
allCards = [dummy, dumber, triDummy, dumbo, bigDumbo, kingDumbo]

dummy :: Card
dummy = Card Dummy 1 3 1 1

dumber :: Card
dumber = Card Dumber 2 3 2 2

triDummy :: Card
triDummy = Card TriDummy 3 3 3 3

dumbo :: Card
dumbo = Card Dumbo 4 3 4 4

bigDumbo :: Card
bigDumbo = Card BigDumbo 5 3 5 5

kingDumbo :: Card
kingDumbo = Card KingDumbo 6 3 6 6

