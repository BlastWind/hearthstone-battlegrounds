{-# LANGUAGE TemplateHaskell #-}

module Model (module Model) where

import Control.Lens
import Data.Map (Map)

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

data TargetSelection = RandomTarget | LeftmostTarget | RightmostTarget | SpecificTarget Model.Index
  deriving (Eq, Show)

data CombatEffectContext = CombatEffectContext {_combatState :: CombatState, _fromFighter :: Fighter, _fromId :: MinionID}

data CardEffect
  = Summon CardCriteria
  | DealDamage Int TargetSelection
  deriving (Eq, Show)

data Card = Card
  { _cardName :: CardName,
    _cardTier :: TavernTier,
    _baseCost :: CardCost,
    _attack :: Attack,
    _health :: Health,
    _deathrattle :: [CardEffect]
  }
  deriving (Eq, Show)

type MinionID = Int

newtype IdGen = IdGen {_unIdGen :: MinionID} deriving (Eq, Show)

data CardInstance = CardInstance
  { _card :: Card,
    _id :: MinionID
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

data Config = Config {_maxBoardSize :: Int, _maxHandSize :: Int, _maxCombatBoardSize :: Int} deriving (Eq, Show)

data CombatSimulation = CombatSimulation
  { _combatMoves :: [CombatMove],
    _boardSequences :: [(Board, Board)],
    _result :: CombatResult
  }
  deriving (Show)

-- TODO: The client can replay the same combat if provided the same seed
-- However, for testing purposes, it will be nice to manually write out the attack sequence
data CombatMove
  = Attack Int Int -- Player1's ith minion attacks Player2's jth minion;
  deriving (Show)

type NextAttackIndex = Int -- When player becomes Attacker, which of their minion attacks next?

data FighterState = FighterState
  { _fplayerState :: PlayerState, -- so that we can perform effects (add cards to hand), deal damage to players, etc
    _nextAttackIndex :: NextAttackIndex
  }
  deriving (Eq, Show)

data CombatState = CombatState {_attacker :: Fighter, _one :: FighterState, _two :: FighterState, _cconfig :: Config} deriving (Eq, Show)

data Fighter = One | Two deriving (Show, Eq)

data CombatResult
  = Loss Fighter Damage -- loser
  | Tie
  deriving (Show, Eq)

type Damage = Int

type CombatHistory = [(Board, Board)]

data PlayerState = PlayerState
  { _tier :: TavernTier,
    _maxGold :: Gold,
    _curGold :: Gold,
    _tierUpCost :: Gold,
    _shop :: Shop,
    _board :: Board,
    _hand :: Hand,
    _frozen :: Bool,
    _hp :: Health,
    _armor :: Armor,
    _alive :: Bool,
    _rerollCost :: Gold,
    _phase :: Phase,
    _idGen :: IdGen
  }
  deriving (Eq, Show)

defPlayerState :: PlayerState
defPlayerState = PlayerState {
  _tier = 0,
  _maxGold = 0, 
  _curGold = 0,
  _tierUpCost = 0,
  _shop = [],
  _frozen = False,
  _board = [],
  _hand = [],
  _hp = 10,
  _armor = 0,
  _alive = True,
  _rerollCost = 1,
  _phase = Recruit,
  _idGen = IdGen 0
}

type PlayerId = Int

data GameState = GameState
  { _playerMap :: Map PlayerId PlayerState,
    _config :: Config,
    _turn :: Turn
  }
  deriving (Eq, Show)

type Index = Int

type DefenderIndex = Model.Index

type AttackerIndex = Model.Index

data Command
  = Buy Model.Index
  | Sell Model.Index
  | Play Model.Index
  | Roll
  | TierUp
  | Freeze
  | EndTurn
  | Help
  | Concede

-- Generate lenses for all records
makeLenses ''Card
makeLenses ''CardInstance  
makeLenses ''PlayerState
makeLenses ''GameState
makeLenses ''Config
makeLenses ''CombatState
makeLenses ''CombatEffectContext
makeLenses ''FighterState
