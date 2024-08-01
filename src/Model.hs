{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Model (module Model) where

import Control.Lens hiding (Index)
import Data.UUID (UUID)
import System.Random (StdGen)

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

data CardName = Dummy | Dumber | TriDummy | Dumbo | BigDumbo | KingDumbo | DummyWithALongNameItKeepsGoing deriving (Show)

data Card = Card
  { _cardName :: CardName,
    _cardTier :: TavernTier,
    _baseCost :: CardCost,
    _attack :: Attack,
    _health :: Health
  }

data CardInstance = CardInstance
  { _cardId :: UUID,
    _card :: Card
  }

$(makeLenses ''Card)

type Gold = Int

type Hand = [CardInstance]

type Shop = [CardInstance]

type Board = [CardInstance]

type PlayerHP = Int

type Turn = Int -- What turn are we on?

type UserName = String

data Phase = HeroSelect | Recruit | Combat deriving (Eq)

-- For now, GameState just keeps track of the solo player and one AI.
data Player = Player | AI
data GameState = GameState
  { playerState :: PlayerState, 
    aiState :: PlayerState,
    turn :: Turn
  }

data CombatMoves = CombatMoves
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
    combatSequence :: ([CombatMoves], Int)
  }

data Env = Env
  { gen :: StdGen
  }

type Index = Int

data GameAction = StartGame

data Command = EndTurn | Help | Buy Index | Sell Index | Play Index