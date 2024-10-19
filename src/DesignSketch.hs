-- Temp file. Place for some all-in-one mocks

module TempDesignSketch (module TempDesignSketch) where

-- Sources of randomness:
-- 1. Random defender selection
-- 2. Random shop rolls
-- 3. Choose random minion in tavern (picky eater, enchanted lasso)
-- 4. Choose random minion in opponent's warband (cry foul)
-- 4. Choose random card under some additional criterion (chef's choice, shell whistler)

type Multiplier = Int

data RandomTarget
  = HandRandom
  | HandLeftmost
  | HandRightmost
  | BoardRandom
  | ShopRandom
  deriving (Eq)

data RandomCriterion = RTribe Tribe | RTarget RandomTarget | RTier Int deriving (Eq)

data CounterCriterion
  = -- Upbeat Frontdrake
    EndOfTurn
  | -- Avenge mechanic
    FriendlyDeaths
  deriving (Eq)

-- Unused yet.
data Target
  = TShop Int
  | THand Int
  | -- Them Apples.
    TEntireShop

data TargetedEffect -- TODO: The idea of a target might be important.

-- TODO: Something is missing.
--

data InjectAvatar cont
  = QueryTier (Int -> cont)
  | QueryHealth (Int -> cont)

data StateEffect
  = -- E.g., Glim Guardian
    GainStats Int Int
  | -- E.g., Cord Puller
    Summon CardName
  | -- E.g., Backstage Security
    DamageHero Int
  | -- E.g., Upbeat Frontdrake
    GetRandom [RandomCriterion]
  | -- E.g., Geomancer
    Get CardName
  | -- E.g., Picky Eater
    Consume Multiplier
  | -- E.g., Tavern Coin
    GainGold Int
  deriving (Eq)

data Tribe = Murloc | Dragon | Demon deriving (Eq)

-- TODO: It has became obvious what are "keywords" and what are "functionalities". Just observe who has StateEffect.
data Functionality
  = -- phase=Combat
    Taunt
  | DivineShield
  | Deathrattle [StateEffect]
  | OnAttack [StateEffect]
  | OnDamaged [StateEffect]
  | OnKill [StateEffect]
  | OnSummon [StateEffect]
  | OnSell [StateEffect]
  | StartOfCombat [StateEffect]
  | Reborn
  | Windfury
  | -- phase=Recruit
    Battlecry StateEffect
  | AfterPlay [StateEffect]
  | Spellcraft Card
  | -- Both
    AfterSummon Tribe StateEffect
  | -- Combinator
    Counter CounterCriterion Int StateEffect
  deriving (Eq)

data Stats = Stats Int Int deriving (Eq)

data CardName
  = GlimGuardian
  | HarmlessBonehead
  | CordPuller
  | Skeleton
  | Microbot
  | UpbeatFrontdrake
  | EnchantedLasso
  | MisfitDragonling
  deriving (Eq)

data Card = Card CardName Stats [Functionality] deriving (Eq)

newtype CardInstance = CardInstance Card

glimGuardian :: Card
glimGuardian = Card GlimGuardian (Stats 1 4) [OnAttack [GainStats 2 1]]

skeleton :: Card
skeleton = Card Skeleton (Stats 1 1) []

harmlessBonehead :: Card
harmlessBonehead = Card HarmlessBonehead (Stats 1 1) [Deathrattle [Summon Skeleton, Summon Skeleton]]

microbot :: Card
microbot = Card Microbot (Stats 1 1) []

cordPuller :: Card
cordPuller = Card CordPuller (Stats 1 1) [DivineShield, Deathrattle [Summon Microbot]]

upbeatFrontdrake :: Card
upbeatFrontdrake = Card UpbeatFrontdrake (Stats 1 1) [Counter EndOfTurn 3 (GetRandom [RTribe Dragon])]

-- Look mom! Tavern spells can be modeled as cards.
enchantedLasso :: Card
enchantedLasso = Card EnchantedLasso (Stats 0 0) [Battlecry (GetRandom [RTarget ShopRandom])]

misfitDragonling :: Card
misfitDragonling = Card MisfitDragonling (Stats 2 1) [StartOfCombat [GainStats]]

data GameState
  = GameState
  { p1 :: [Card],
    p2 :: [Card],
    onSummonCallbacks :: [GameState -> GameState],
    onPlayCallbacks :: [GameState -> GameState]
  }

trade :: (CardInstance, CardInstance) -> (CardInstance, CardInstance)
trade (CardInstance (Card name1 stats1 fns1), CardInstance (Card name2 stats2 fns2)) = (_, _)
  where
    ds1 = DivineShield `elem` fns1