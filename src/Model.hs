module Model (module Model) where

import Data.UUID (UUID)

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
  { cardName :: CardName,
    cardTier :: TavernTier,
    baseCost :: CardCost,
    attack :: Attack,
    health :: Health
  }

data CardInstance = CardInstance
  { cardId :: UUID,
    card :: Card
  }

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

newtype Config = Config { maxBoardSize :: Int }
data GameState = GameState
  { playerState :: PlayerState,
    aiState :: PlayerState,
    config :: Config,
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

type Index = Int

data Command
  = Buy Index
  | Sell Index
  | Play Index
  | Roll
  | Freeze
  | EndTurn
  | Help
  | Concede