{-# LANGUAGE DuplicateRecordFields #-}

module CombatTest (module CombatTest) where

import Card
import Data.Maybe (fromJust)
import Model
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (assertEqual, testCase)

combatTestGroup :: TestTree
combatTestGroup =
  testGroup
    "Combat Tests"
    [ testHarmlessBonehead
    ]

testHarmlessBonehead :: TestTree
testHarmlessBonehead =
  testCase "Expected combat state after player Harmless Bonehead attacks enemy Harmless Bonehead and dies." $
    assertEqual "Combat state" before before
  where
    before :: CombatState
    before =
      CombatState
        { attacker = ContestantState {nextAttackIndex = 0, contestant = One, board = freeInstances [harmlessBonehead]},
          defender = ContestantState {nextAttackIndex = 0, contestant = Two, board = freeInstances [harmlessBonehead]}
        }

freeInstances :: [Card] -> [CardInstance]
freeInstances cs = [CardInstance c | c <- cs]

-- blankPlayerState :: PlayerState
-- blankPlayerState =
--   PlayerState
--     { shop = [],
--       board = [],
--       hand = [],
--       frozen = False,
--       rerollCost = 1,
--       tierUpCost = 5,
--       hp = 30,
--       armor = 5,
--       curGold = 7,
--       maxGold = 10,
--       tier = 1,
--       phase = Recruit,
--       alive = True
--     }

-- blankAIState :: PlayerState
-- blankAIState = blankPlayerState {hp = 5, armor = 0}

-- blankGameState :: GameState
-- blankGameState = GameState {playerState = blankPlayerState, aiState = blankAIState, config = Config {maxBoardSize = 7, maxHandSize=10}, turn = 0}
