{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE TemplateHaskell #-}

module Model (module Model) where

import Control.Lens
import Control.Lens.TH
import Data.Map (Map)
import Data.UUID (UUID)
import System.Random (StdGen)

{-
Design Philosophy:
The internals of battlegrounds can be modelled as an algebra on GameState.
-}

type Attack = Int

type Health = Int

type TierUpCost = Int

type TavernTier = Int

type CardCost = Int

data CardName = Dummy | Dumber | TriDummy | Dumbo | BigDumbo | KingDumbo

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

data Phase = HeroSelect | Blank | Recruit | Combat

data GameState = GameState
  { playerStates :: Map UserName PlayerState,
    turn :: Turn
  }

data PlayerState = PlayerState
  { tier :: TavernTier,
    maxGold :: Gold,
    curGold :: Gold,
    tierUpCost :: Gold,
    shop :: Shop,
    board :: Board,
    hand :: Hand,
    phase :: Phase,
    frozen :: Bool,
    hp :: Health
  }


data Env = Env {
  gen :: StdGen
}