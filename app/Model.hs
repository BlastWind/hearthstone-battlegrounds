{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Model (GameState, PlayerState(..), Event(..), EventHandler(..)) where
import Control.Monad.State.Lazy (State)
import Data.Map (Map, empty)
import Data.UUID (UUID)
{- 
Design Philosophy: 
The internals of battlegrounds can be modelled as an algebra on GameState.
-}

-- ##### BEGIN: Enums ##### 
data CardName = AnnoyOTron | BackStageSecurity | CordPuller | DeckSwabbie | DeepSeaAngler | GlimGuardian | HarmlessBonehead | Manasaber | Peggy
data Tribe = Quilboar | Pirate | Mech | Elemental | Dragon | Naga | Undead | Beast
-- ##### END:   Enums #####

data Card = Card { 
    cardId :: UUID,
    cardName :: CardName, 
    tier :: TavernTier, 
    tribe :: Tribe,
    attack :: Attack, 
    health :: Health, 
    battlecry :: PlayerState -> PlayerState, 
    deathrattle :: PlayerState -> PlayerState,
    onSell :: PlayerState -> PlayerState,
    onBuy  :: PlayerState -> PlayerState,
    startOfTurn :: PlayerState -> PlayerState,
    endOfTurn :: PlayerState -> PlayerState
    -- counter :: State Int Int -- for seafarer, malchezaar, chimera, etc.

    -- Some cards have very special effects when a tangential action occurs
    -- Examples: 
    -- Peggy buffs minion when any card is added to hand
    -- Swampstriker is buffed when murloc is player
    -- Seafarer, Malchezaar, Bazaar Dealer needs a counter for its effects
    -- In combat, chimera, skyblazer can buff minions permanently
}

gainGold :: Int -> (PlayerState -> PlayerState)
gainGold = undefined
-- gainGold i gs  = GameState { ...gs, gold = gold + i }


peggy :: Card
peggy = Card {
    cardName = Peggy,
    tier = TavernTier 4,
    attack = Attack 4,
    health = Health 2,
    battlecry = id,
    deathrattle = id,
    onSell = gainGold 1
}

newtype Attack = Attack Int
newtype Health = Health Int
newtype TierUpCost = TierUpCost Int deriving (Num)
newtype TavernTier = TavernTier Int
newtype Gold = Gold Int deriving (Num)
newtype Hand = Hand [Card]
newtype Shop = Shop [Card]
newtype Board = Board [Card]
newtype PlayerHP = PlayerHP Int
newtype Turn = Turn Int -- What turn are we on?
newtype UserName = UserName String
newtype Pool = Pool (Map CardName Int)

data GameState = GameState { 
    turn :: Turn,
    playerStates :: Map UserName PlayerState,
    sharedPool :: Map CardName Int
}

data PlayerState = PlayerState {
    -- START: Obvious Properties
    tierUpCost :: Integer, 
    tavernTier :: Integer, 
    playerHP :: PlayerHP, 
    hand :: Hand, 
    shop :: Shop, 
    board :: Board, 
    curGold :: Integer,
    maxGold :: Integer,
    -- END: Obvious Properties

    -- START: Inobvious properties or ones for implementational purposes (as opposed for pure battleground rules)
    -- Why is rollCosts an list of effects? Why isn't it encoded within `roll` and simply subtract 1 gold?
    -- This is because of cards like Malchezaar and Recycling Wraith.
    rollCosts :: [PlayerState -> PlayerState],
    eventHandler :: EventHandler
    -- END: Inobvious or for implementational purposes
}

initialGameState :: GameState
initialGameState = undefined


data Event
    = HealthChange
    | CardAdded
    | Refresh
    | Sell
    | Buy
    | Reroll
    | Freeze
    | TierUp
    deriving (Eq, Ord) -- Ord instance is just ease of Data.Map integration.


type EventHandler = Map Event [PlayerState -> PlayerState]

initialHandler :: EventHandler
initialHandler =  empty

-- game = until onePlayerAlive (combatPhase . shopPhase) initialGameState