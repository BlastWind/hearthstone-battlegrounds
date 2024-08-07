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
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE UndecidableInstances #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Model (module Model) where

import Data.Record.Plugin
import Prelude

{-
Design Philosophy:
The internals of battlegrounds can be modelled as an algebra on GameState.
-}

type Attack = Int

type Health = Int

type Armor = Int

type TierUpCost = Int

type TavernTier = Int

type CardCost = Int

data CardName
  = PlaceHolder
  | Skeleton
  | HarmlessBonehead
  | Dummy
  | Dumber
  | TriDummy
  | Dumbo
  | BigDumbo
  | KingDumbo
  | DummyWithALongNameItKeepsGoing
  deriving (Eq, Show)

data Keyword = Deathrattle deriving (Eq, Show)
data CardCriteria = SpecificCard Card | ByKeyword Keyword
  deriving (Eq, Show)

data TargetSelection = RandomTarget | LeftmostTarget | RightmostTarget | SpecificTarget Index
  deriving (Eq, Show)

data CombatEffectContext = CombatEffectContext {combatState :: CombatState, fromFighter :: Fighter, fromId :: MinionID}

data CardEffect
  = Summon CardCriteria
  | DealDamage Int TargetSelection
  deriving (Eq, Show)

{-# ANN type Card largeRecord #-}
data Card = Card
  { cardName :: CardName,
    cardTier :: TavernTier,
    baseCost :: CardCost,
    attack :: Attack,
    health :: Health,
    deathrattle :: [CardEffect]
  }
  deriving (Eq, Show)

type MinionID = Int

newtype IdGen = IdGen {unIdGen :: MinionID} deriving (Show)

{-# ANN type CardInstance largeRecord #-}
data CardInstance = CardInstance
  { card :: Card,
    id :: MinionID
  }
  deriving (Eq, Show)

type Gold = Int

type Hand = [CardInstance]

type Shop = [CardInstance]

type Board = [CardInstance]

type PlayerHP = Int

type Turn = Int -- What turn are we on?

type UserName = String

data Phase = HeroSelect | Recruit | Combat | EndScreen deriving (Show, Eq)

-- For now, GameState just keeps track of the solo player and one AI.
data Player = Player | AI deriving (Show, Eq)

{-# ANN type Config largeRecord #-}
data Config = Config {maxBoardSize :: Int, maxHandSize :: Int, maxCombatBoardSize :: Int} deriving (Show)

{-# ANN type CombatSimulation largeRecord #-}
data CombatSimulation = CombatSimulation
  { combatMoves :: [CombatMove],
    boardSequences :: [(Board, Board)],
    result :: CombatResult
  }
  deriving (Show)

-- TODO: The client can replay the same combat if provided the same seed
-- However, for testing purposes, it will be nice to manually write out the attack sequence
data CombatMove
  = Attack Int Int -- Player1's ith minion attacks Player2's jth minion;
  deriving (Show)

type NextAttackIndex = Int -- When player becomes Attacker, which of their minion attacks next?

{-# ANN type FighterState largeRecord #-}
data FighterState = FighterState
  { playerState :: PlayerState, -- so that we can perform effects (add cards to hand), deal damage to players, etc
    nextAttackIndex :: NextAttackIndex
  }
  deriving (Show)

{-# ANN type CombatState largeRecord #-}
data CombatState = CombatState {attacker :: Fighter, one :: FighterState, two :: FighterState, config :: Config} deriving (Show)

data Fighter = One | Two deriving (Show, Eq)

data CombatResult
  = Loss Fighter Damage -- loser
  | Tie
  deriving (Show, Eq)

type Damage = Int

type CombatHistory = [(Board, Board)]

{-# ANN type PlayerState largeRecord #-}
data PlayerState = PlayerState
  { tier :: TavernTier,
    maxGold :: Gold,
    curGold :: Gold,
    tierUpCost :: Gold,
    shop :: Shop,
    board :: Board,
    hand :: Hand,
    frozen :: Bool,
    hp :: Health,
    armor :: Armor,
    alive :: Bool,
    rerollCost :: Gold,
    phase :: Phase,
    idGen :: IdGen
  }
  deriving (Show)

{-# ANN type GameState largeRecord #-}
data GameState = GameState
  { playerState :: PlayerState,
    aiState :: PlayerState,
    config :: Config,
    turn :: Turn
  }
  deriving (Show)

type Index = Int

type DefenderIndex = Index

type AttackerIndex = Index

data Command
  = Buy Index
  | Sell Index
  | Play Index
  | Roll
  | TierUp
  | Freeze
  | EndTurn
  | Help
  | Concede