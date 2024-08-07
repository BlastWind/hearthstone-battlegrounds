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
  testCase "Expected combat state after player Harmless Bonehead attacks enemy Harmless Bonehead and dies." $ do
    assertEqual
      "Attack indices update correctly after bonehead death + summon, boneheads are the only minions."
      after
      (snipe 0 before)
    assertEqual
      "Attack indices update correctly after bonehead death + summon, when other minions exist before bonehead."
      after'
      (snipe 1 before')
  where
    before =
      CombatState
        { attacker = FighterState {nextAttackIndex = 0, fighter = One, board = [CardInstance harmlessBonehead]},
          defender = FighterState {nextAttackIndex = 0, fighter = Two, board = [CardInstance harmlessBonehead]}
        }
    after =
      CombatState
        { attacker = FighterState {nextAttackIndex = 0, fighter = One, board = [CardInstance skeleton, CardInstance skeleton]},
          defender = FighterState {nextAttackIndex = 0, fighter = Two, board = [CardInstance skeleton, CardInstance skeleton]}
        }
    before' =
      CombatState
        { attacker = FighterState {nextAttackIndex = 1, fighter = One, board = [CardInstance dummy, CardInstance harmlessBonehead]},
          defender = FighterState {nextAttackIndex = 1, fighter = Two, board = [CardInstance dummy, CardInstance harmlessBonehead]}
        }
    after' =
      CombatState
        { attacker = FighterState {nextAttackIndex = 1, fighter = One, board = [CardInstance dummy, CardInstance skeleton, CardInstance skeleton]},
          defender = FighterState {nextAttackIndex = 1, fighter = Two, board = [CardInstance dummy, CardInstance skeleton, CardInstance skeleton]}
        }