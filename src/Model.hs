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
  } deriving Show

data CardInstance = CardInstance
  { cardId :: UUID,
    card :: Card
  } deriving Show

type Gold = Int

type Hand = [CardInstance]

type Shop = [CardInstance]

type Board = [CardInstance]

type PlayerHP = Int

type Turn = Int -- What turn are we on?

type UserName = String

data Phase = HeroSelect | Recruit | Combat | EndScreen deriving (Eq)

-- For now, GameState just keeps track of the solo player and one AI.
data Player = Player | AI deriving (Show, Eq)

data Config = Config { maxBoardSize :: Int, maxHandSize :: Int }
data GameState = GameState
  { playerState :: PlayerState,
    aiState :: PlayerState,
    config :: Config,
    turn :: Turn
  }

data CombatSimulation = CombatSimulation {combatMoves :: [CombatMove], boardSequences :: [(Board, Board)]} deriving Show

-- TODO: The client can replay the same combat if provided the same seed
-- However, for testing purposes, it will be nice to manually write out the attack sequence
data CombatMove = 
  Attack Int Int -- Player1's ith minion attacks Player2's jth minion; 
  deriving Show

data Contestant = One | Two deriving (Show, Eq)
data CombatResult = Loser Contestant | Tie deriving (Show, Eq)
type Damage = Int
type CombatHistory = [(Board, Board)]

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
    combatSimulation :: CombatSimulation
  }

type Index = Int

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