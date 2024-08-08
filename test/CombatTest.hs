{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module CombatTest (module CombatTest) where

import Card
import Combat
import Control.Arrow (second)
import Data.Record.Overloading hiding (loop)
import Model hiding (turn)
import Test.Tasty (TestTree, testGroup)
import Test.Tasty.HUnit (Assertion, assertEqual, testCase)

combatTestGroup :: TestTree
combatTestGroup =
  testGroup
    "test basic combats via histories"
    [ assert1v1HarmlessBonehead
    ]

type HardCodedTurn = (CombatState -> (CombatState, CombatHistory)) -- See signature of `turn`.

-- Frequently, we wish to test the expected history after a series of turns. 
-- There is a tradeoff with testing history versus testing combat states:
-- Pro: Easier to write
-- Con: Slightly less rigorous. Harder to localize error (at which turn did state go wrong?)
makeMultiturnHistoryAssertionTest :: CombatHistory -> CombatState -> [HardCodedTurn] -> Assertion
makeMultiturnHistoryAssertionTest expectedHistory cs turns =
  assertEqual
    "Expected same history"
    expectedHistory
    ( snd -- discard CombatState since we just want history
        $ foldl
          (\(cs, histAcc) t -> second (histAcc ++) (t cs)) --
          (cs, []) -- initial state and the history accumulator.
          turns
    )

assert1v1HarmlessBonehead :: TestTree
assert1v1HarmlessBonehead =
  testCase "Two boneheads full battle" $ makeMultiturnHistoryAssertionTest expectedHistory initialCS turns
  where
    turns = [turn 0, turn 1, turn 0]
    onePS =
      defPlayerState
        { phase = Combat,
          board = [CardInstance harmlessBonehead 0],
          idGen = IdGen 1
        }
    twoPS =
      defPlayerState
        { phase = Combat,
          board = [CardInstance harmlessBonehead 0],
          idGen = IdGen 1
        }
    initialCS =
      CombatState
        { attacker = One,
          one = FighterState {nextAttackIndex = 0, playerState = onePS},
          two = FighterState {nextAttackIndex = 0, playerState = twoPS},
          config = Config {maxBoardSize = 7, maxHandSize = 10, maxCombatBoardSize = 7}
        }
    expectedHistory =
      [ ([CardInstance harmlessBonehead 0], [CardInstance harmlessBonehead 0]),
        ([CardInstance harmlessBonehead {health = 0} 0], [CardInstance harmlessBonehead {health = 0} 0]),
        ([CardInstance skeleton 1], [CardInstance skeleton 1]),
        ([CardInstance skeleton 1, CardInstance skeleton 2], [CardInstance skeleton 1, CardInstance skeleton 2]),
        ([CardInstance skeleton 1, CardInstance skeleton {health = 0} 2], [CardInstance skeleton {health = 0} 1, CardInstance skeleton 2]),
        ([CardInstance skeleton 1], [CardInstance skeleton 2]),
        ([CardInstance skeleton {health = 0} 1], [CardInstance skeleton {health = 0} 2]),
        ([], [])
      ]