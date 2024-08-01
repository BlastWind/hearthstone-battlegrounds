-- View tests are regressive (ensure that new changes do not inadvertently regress the existing UI).
module ViewTest (viewTestGroup) where

import Card
import Data.List (intercalate)
import Data.Maybe (fromJust)
import Data.UUID (UUID, fromString)
import Model (CardInstance (..), GameState (..), Phase (..), PlayerState (..), Player (Player))
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)
import View (render)

viewTestGroup :: TestTree
viewTestGroup =
  testGroup
    "View Tests"
    [ testBlankRecruitView,
      testRecruitViewWithMaxItems,
      testRecruitViewWithAbbrevs
    ]

testBlankRecruitView :: TestTree
testBlankRecruitView =
  testCase "Test view of blank shop, board, hand. Other hard coded stats can test stat renderings." $
    assertEqual "view should be eq" (render blankGameState Player) $
      intercalate
        "\n"
        [ "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "|                                                                   Recruit                                                                  |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Shop:                                                                                                                                      |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Board:                                                                                                                                     |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Hand:                                                                                                                                      |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Tavern:                                          Upgrade Cost: 5 | Freeze: Yes | Reroll Cost: 2                                            |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Player:                                                Health: 30 | Armor: 5 | Gold: 7/10                                                  |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Opps HP:                                                       Tutorial AI: 5 + 0                                                          |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+"
        ]

testRecruitViewWithMaxItems :: TestTree
testRecruitViewWithMaxItems =
  testCase "Test view of filled shop, board, hand and max opps." $
    assertEqual "view should be eq" (render maxItemsGameState Player) $
      intercalate
        "\n"
        [ "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "|                                                                   Recruit                                                                  |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Shop:                                       Dummy | Dumber | TriDummy | Dumbo | BigDumbo | KingDumbo                                       |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Board:                                      Dummy | Dumber | TriDummy | Dumbo | BigDumbo | KingDumbo                                       |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Hand:                                 Dummy | Dumber | TriDummy | Dumbo | BigDumbo | KingDumbo | KingDumbo                                 |",
          "|                                                        KingDumbo | KingDumbo | KingDumbo                                                   |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Tavern:                                          Upgrade Cost: 5 | Freeze: Yes | Reroll Cost: 2                                            |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Player:                                                Health: 30 | Armor: 5 | Gold: 7/10                                                  |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Opps HP:                                                       Tutorial AI: 5 + 0                                                          |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+"
        ]

testRecruitViewWithAbbrevs :: TestTree
testRecruitViewWithAbbrevs =
  testCase "Test view of filled shop, board, hand and max opps. All names chosen to exceed limit (and hence should be abbreviated)." $
    assertEqual "view should be eq" (render maxItemsWithAbbrevNamesGameState Player) $
      intercalate
        "\n"
        [ "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "|                                                                   Recruit                                                                  |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Shop:     DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon..      |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Board:    DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon..      |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Hand:     DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon.. | DummyWithALon..      |",
          "|                                               DummyWithALon.. | DummyWithALon.. | DummyWithALon..                                          |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Tavern:                                          Upgrade Cost: 5 | Freeze: Yes | Reroll Cost: 2                                            |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Player:                                                Health: 30 | Armor: 5 | Gold: 7/10                                                  |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+",
          "| Opps HP:                                                       Tutorial AI: 5 + 0                                                          |",
          "+--------------------------------------------------------------------------------------------------------------------------------------------+"
        ]

dummyUUID :: UUID
dummyUUID = fromJust $ fromString "c2cc10e1-57d6-4b6f-9899-38d972112d8c"

blankPlayerState :: PlayerState
blankPlayerState =
  PlayerState
    { shop = [],
      board = [],
      hand = [],
      frozen = True,
      rerollCost = 2,
      tierUpCost = 5,
      hp = 30,
      armor = 5,
      curGold = 7,
      maxGold = 10,
      tier = 5,
      phase = Recruit,
      alive = True,
      combatSequence = ([], 0)
    }

blankAIState :: PlayerState
blankAIState = blankPlayerState {hp = 5, armor = 0}

blankGameState :: GameState
blankGameState = GameState {playerState = blankPlayerState, aiState = blankAIState, turn = 0}

maxItemsGameState :: GameState
maxItemsGameState = blankGameState {playerState = maxItemsPlayerState}
  where
    maxItemsPlayerState =
      blankPlayerState
        { shop = [CardInstance dummyUUID dummy, CardInstance dummyUUID dumber, CardInstance dummyUUID triDummy, CardInstance dummyUUID dumbo, CardInstance dummyUUID bigDumbo, CardInstance dummyUUID kingDumbo],
          board = [CardInstance dummyUUID dummy, CardInstance dummyUUID dumber, CardInstance dummyUUID triDummy, CardInstance dummyUUID dumbo, CardInstance dummyUUID bigDumbo, CardInstance dummyUUID kingDumbo],
          hand = [CardInstance dummyUUID dummy, CardInstance dummyUUID dumber, CardInstance dummyUUID triDummy, CardInstance dummyUUID dumbo, CardInstance dummyUUID bigDumbo, CardInstance dummyUUID kingDumbo, CardInstance dummyUUID kingDumbo, CardInstance dummyUUID kingDumbo, CardInstance dummyUUID kingDumbo, CardInstance dummyUUID kingDumbo]
        }

maxItemsWithAbbrevNamesGameState :: GameState
maxItemsWithAbbrevNamesGameState = blankGameState {playerState = maxItemsPlayerState}
  where
    maxItemsPlayerState =
      blankPlayerState
        { shop = replicate 7 $ CardInstance dummyUUID dummyWithALongNameItKeepsGoing,
          board = replicate 7 $ CardInstance dummyUUID dummyWithALongNameItKeepsGoing,
          hand = replicate 10 $ CardInstance dummyUUID dummyWithALongNameItKeepsGoing
        }