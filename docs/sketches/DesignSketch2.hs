{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeFamilies #-}

module DesignSketch2 where
  
import Control.Monad.Except
import Control.Monad.Reader
import Effectful
import Effectful.Error.Static
import Effectful.Dispatch.Dynamic

data Tribe = Murloc | Dragon | Demon | Elemental | Undead | Mech | Naga | MurlocDragon | All | SpellTODO deriving (Eq)

data CounterType
  = -- Upbeat Frontdrake
    EndOfTurn
  | -- Avenge mechanic
    FriendlyDeaths
  | -- Tehhys
    GoldSpent
  | -- Elise
    Refresh
  deriving (Eq)

data CardFilterCriterion = MaxTier Int | Tribe Tribe | IsMinion | NotSelf

data RandomTarget = Hand | Shop | Board

data Per = PerCombat | PerRecruit | PerGame

type EffectError = String

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
  | MoltenRock
  | PickyEater
  | DeepseaAngler
  | AnglersLure
  | SnailCavalry
  | RecruitATrainee
  | BlazingSkyfin
  | AncestralAutomaton
  | BrannBronzebeard
  | DummyCard
  deriving (Eq)

data StateEffect
  = -- Stats that will be permanently gained even during combat. E.g., Trusty Pup
    GainPermStats Stats
  | -- Stats that will be permanent if gained during recruit, and temporary if gained during combat. E.g., Blazing Skyfin
    GainStats Stats
  | -- Stats that is only temporarily gained no matter what. E.g., Spellcraft.
    GainTempStats Stats
  | -- Stats that is permanently gained for all future instances. E.g., Ancestral Automaton, Eternal Knight
    GainStatsForAll Stats
  | -- Reserved for deep blue. TODO: Is there a way to factor this into the current gain stat schemes?
    GainStatsDeepBlue Stats
  | GainTempTaunt
  | GainTaunt
  | -- E.g., Cord Puller
    Summon CardName
  | -- E.g., Backstage Security
    DamageHero Int
  | -- E.g., Upbeat Frontdrake
    AddToHand CardInstance
  | -- E.g., Lasso
    RemoveFromShop CardInstance
  | Take CardInstance
  | -- E.g., Tavern Coin
    GainGold Int
  | -- E.g., Brann, Dreamer's Embrace
    TriggerBattlecry CardInstance

data KeywordFunctionality
  = Taunt
  | DivineShield
  | Reborn
  | Windfury
  | Deathrattle (forall es. (Avatar :> es) => Eff es [StateEffect])
  | StartOfCombat (forall es. (Avatar :> es) => Eff es [StateEffect])
  | Battlecry (forall es. (Avatar :> es) => Eff es [StateEffect])
  | Spellcraft Card

type Count = Int

data EventFunctionality
  = OnAttack (forall es. (Avatar :> es) => Eff es [StateEffect])
  | OnDamaged (forall es. (Avatar :> es) => Eff es [StateEffect])
  | OnKill (forall es. (Avatar :> es) => Eff es [StateEffect])
  | OnSell (forall es. (Avatar :> es) => Eff es [StateEffect])
  | AfterPlay (forall es. (Avatar :> es) => Eff es [StateEffect])
  | AfterSummon (forall es. (Avatar :> es) => Eff es [StateEffect])
  | AfterBattlecryTrigger (forall es. (Avatar :> es) => Eff es [StateEffect])
  | Every Count CounterType (forall es. (Avatar :> es) => Eff es [StateEffect])

data FunctionalityCombinator
  = -- Per `Per`, run effect up to `Count` times.
    UpTo Count Per EventFunctionality

data Functionality
  = Keyword KeywordFunctionality
  | Event EventFunctionality
  | Combinator FunctionalityCombinator

data Card = Card
  { cardName :: CardName,
    stats :: Stats,
    tribe :: Tribe,
    functionality :: [Functionality]
  }

data CardInstance = CardInstance {card :: Card, instanceId :: Int}

data Avatar :: Effect where
  QueryTier :: Avatar m Int
  MakeRandomCard :: [CardFilterCriterion] -> Avatar m CardInstance
  TargetRandomCard :: RandomTarget -> [CardFilterCriterion] -> Avatar m CardInstance
  TargetRandomCards :: RandomTarget -> [CardFilterCriterion] -> Int -> Avatar m [CardInstance]
  RetrieveAssociatedCard :: Avatar m CardInstance
  RetrieveBoard :: Avatar m [CardInstance]

type instance DispatchOf Avatar = Dynamic


queryTier :: Avatar :> es => Eff es Int
queryTier = send QueryTier

makeRandomCard :: Avatar :> es => [CardFilterCriterion] -> Eff es CardInstance
makeRandomCard = send . MakeRandomCard

targetRandomCard :: Avatar :> es => RandomTarget -> [CardFilterCriterion] -> Eff es CardInstance
targetRandomCard target = send . TargetRandomCard target

targetRandomCards :: Avatar :> es => RandomTarget -> [CardFilterCriterion] -> Int -> Eff es [CardInstance]
targetRandomCards target criteria = send . TargetRandomCards target criteria

retrieveAssociatedCard :: Avatar :> es => Eff es CardInstance
retrieveAssociatedCard = send RetrieveAssociatedCard

retrieveBoard :: Avatar :> es => Eff es [CardInstance]
retrieveBoard = send RetrieveBoard

data GameState = GameState {}

newtype GameM a = GameM (ReaderT GameState (RandT StdGen IO) a)
  deriving (Functor, Applicative, Monad, MonadRandom, MonadIO)

data TestConfig = TestConfig
  { mockTier :: Int,
    mockCards :: [Card], -- Default sequence
    mockCardGen :: Int -> Card -- Or generate based on context
  }

newtype TestM a = TestM (ReaderT TestConfig IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

-- Example usage:
runGame :: GameM a -> GameState -> IO a
runGame (GameM m) gs = do
  gen <- newStdGen
  evalRandT (runReaderT m gs) gen

runTest :: TestM a -> TestConfig -> IO a
runTest (TestM m) = runReaderT m

glimGuardian :: Card
glimGuardian = Card GlimGuardian (Stats 1 4) Dragon [Event $ OnAttack (return [GainStats (Stats 2 1)])]

skeleton :: Card
skeleton = Card Skeleton (Stats 1 1) Undead []

harmlessBonehead :: Card
harmlessBonehead = Card HarmlessBonehead (Stats 1 1) Undead [Keyword $ Deathrattle (return [Summon Skeleton, Summon Skeleton])]

microbot :: Card
microbot = Card Microbot (Stats 1 1) Mech []

cordPuller :: Card
cordPuller = Card CordPuller (Stats 1 1) Mech [Keyword DivineShield, Keyword $ Deathrattle (return [Summon Microbot])]

upbeatFrontdrake :: Card
upbeatFrontdrake =
  Card
    UpbeatFrontdrake
    (Stats 1 1)
    Dragon
    [ Event $
        Every
          3
          EndOfTurn
          ( do
              t <- queryTier
              c <- makeRandomCard [MaxTier t, Tribe Dragon]
              return [AddToHand c]
          )
    ]

-- Look mom! Tavern spells can be modeled as a minion. But a Spell type is absolutely needed in later versions
enchantedLasso :: Card
enchantedLasso =
  Card
    EnchantedLasso
    (Stats 0 0)
    SpellTODO
    [ Keyword $
        Battlecry
          ( do
              ci <- targetRandomCard Shop [IsMinion]
              return [Take ci]
          )
    ]

misfitDragonling :: Card
misfitDragonling =
  Card
    MisfitDragonling
    (Stats 2 1)
    Dragon
    [ Keyword $
        StartOfCombat
          ( do
              t <- queryTier
              return [GainStats (Stats t t)]
          )
    ]

anglersLure :: Card
anglersLure =
  Card
    AnglersLure
    (Stats 0 0)
    SpellTODO
    [ Keyword $
        Battlecry
          (return [GainTempStats (Stats 0 2), GainTempTaunt])
    ]

deepseaAngler :: Card
deepseaAngler =
  Card
    DeepseaAngler
    (Stats 2 2)
    Naga
    [ Keyword $ Spellcraft anglersLure
    ]

moltenRock :: Card
moltenRock =
  Card
    MoltenRock
    (Stats 3 3)
    Elemental
    [ Event $
        AfterPlay
          ( do
              c <- retrieveAssociatedCard
              return $ [GainStats (Stats 0 1) | (tribe . card) c == Elemental]
          )
    ]

pickyEater :: Card
pickyEater =
  Card
    PickyEater
    (Stats 1 1)
    Demon
    [ Keyword $
        Battlecry
          ( do
              toEat <- targetRandomCard Shop [IsMinion] -- pickEater's battlecry should fail if there is nothing to eat!
              return [RemoveFromShop toEat, GainStats (stats (card toEat))]
          )
    ]

snailCavalry :: Card
snailCavalry =
  Card
    SnailCavalry
    (Stats 2 2)
    Naga
    [ Combinator $
        UpTo
          1
          PerRecruit
          ( AfterPlay
              ( do
                  c <- retrieveAssociatedCard
                  return [GainStats (Stats 1 1) | (tribe . card) c == SpellTODO]
              )
          )
    ]

recruitATrainee :: Card
recruitATrainee =
  Card
    RecruitATrainee
    (Stats 0 0)
    SpellTODO
    [ Keyword $
        Battlecry
          ( do
              c <- makeRandomCard [MaxTier 1]
              return [AddToHand c]
          )
    ]

blazingSkyfin :: Card
blazingSkyfin =
  Card
    BlazingSkyfin
    (Stats 2 4)
    MurlocDragon
    [ Event $
        AfterBattlecryTrigger (return [GainStats (Stats 1 1)])
    ]

ancestralAutomaton :: Card
ancestralAutomaton =
  Card
    AncestralAutomaton
    (Stats 2 5)
    Mech
    [ Event $
        AfterSummon
          ( do
              c <- retrieveAssociatedCard
              return [GainPermStats (Stats 2 1) | cardName (card c) == AncestralAutomaton]
          )
    ]