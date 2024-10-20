-- Temp file. Place for some all-in-one mocks
{-# LANGUAGE DeriveFunctor #-}

module DesignSketch (module DesignSketch) where

import Control.Monad.Free

data CardFilterCriterion = MaxTier Int | Tribe Tribe | IsMinion

data RandomTarget = Hand | Shop | Board

data InjectAvatarMethod next
  = QueryTier (Int -> next)
  | MakeRandomCard [CardFilterCriterion] (CardInstance -> next)
  | TargetRandomCard RandomTarget [CardFilterCriterion] (Either EffectError CardInstance -> next)
  | TargetRandomCards RandomTarget [CardFilterCriterion] Int (Either EffectError [CardInstance] -> next)
  deriving (Functor)

type InjectAvatar a = Free InjectAvatarMethod a

queryTier :: InjectAvatar Int
queryTier = liftF $ QueryTier id

makeRandomCard :: [CardFilterCriterion] -> InjectAvatar CardInstance
makeRandomCard criterions = liftF $ MakeRandomCard criterions id

targetRandomCard :: RandomTarget -> [CardFilterCriterion] -> InjectAvatar (Either EffectError CardInstance)
targetRandomCard randTarget crits = liftF $ TargetRandomCard randTarget crits id

targetRandomCards :: RandomTarget -> [CardFilterCriterion] -> Int -> InjectAvatar (Either EffectError [CardInstance])
targetRandomCards randomTarget crits count = liftF $ TargetRandomCards randomTarget crits count id

data StateEffect
  = -- E.g., Trusty Pup
    GainPermStats Stats
  | -- E.g., Stats gained during Combat (Glim Guardian); Spellcraft
    GainTempStats Stats
  | -- E.g., Ancestral Automaton, Eternal Knight, (Maybe) Deep Blue
    GainBaseStats Stats
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

data Tribe = Murloc | Dragon | Demon | Elemental | Undead | Mech | Naga | SpellTODO deriving (Eq)

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

type EffectError = String

type Count = Int

data Per = PerCombat | PerRecruit | PerGame

data KeywordFunctionality
  = Taunt
  | DivineShield
  | Reborn
  | Windfury
  | Deathrattle (InjectAvatar (Either EffectError [StateEffect]))
  | StartOfCombat (InjectAvatar (Either EffectError [StateEffect]))
  | Battlecry (InjectAvatar (Either EffectError [StateEffect]))
  | Spellcraft Card

data EventFunctionality
  = -- Events detectable by "inspecting" the card itself
    OnAttack (InjectAvatar (Either EffectError [StateEffect]))
  | OnDamaged (InjectAvatar (Either EffectError [StateEffect]))
  | OnKill (InjectAvatar (Either EffectError [StateEffect]))
  | OnSell (InjectAvatar (Either EffectError [StateEffect]))
  | -- Events that are detectable only by listening onto some other more "global" events
    AfterPlay (Card -> Bool) (InjectAvatar (Either EffectError [StateEffect]))
  | AfterSummon (Card -> Bool) (InjectAvatar (Either EffectError [StateEffect]))
  | -- Every `count` times `counterType` happens, run effects
    Every Count CounterType (InjectAvatar (Either EffectError [StateEffect]))

data FunctionalityCombinator
  = -- Per `Per`, run effect up to `Count` times.
    UpTo Count Per EventFunctionality

data Functionality
  = Keyword KeywordFunctionality
  | Event EventFunctionality
  | Combinator FunctionalityCombinator

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
  deriving (Eq)

data Card = Card
  { cardName :: CardName,
    stats :: Stats,
    tribe :: Tribe,
    functionality :: [Functionality]
  }

newtype CardInstance = CardInstance {card :: Card}

glimGuardian :: Card
glimGuardian = Card GlimGuardian (Stats 1 4) Dragon [Event $ OnAttack (return $ Right [GainTempStats (Stats 2 1)])]

skeleton :: Card
skeleton = Card Skeleton (Stats 1 1) Undead []

harmlessBonehead :: Card
harmlessBonehead = Card HarmlessBonehead (Stats 1 1) Undead [Keyword $ Deathrattle (return $ Right [Summon Skeleton, Summon Skeleton])]

microbot :: Card
microbot = Card Microbot (Stats 1 1) Mech []

cordPuller :: Card
cordPuller = Card CordPuller (Stats 1 1) Mech [Keyword DivineShield, Keyword $ Deathrattle (return $ Right [Summon Microbot])]

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
              card <- makeRandomCard [MaxTier t, Tribe Dragon]
              return $ Right [AddToHand card]
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
              either (return . Left) (\ci -> return $ Right [Take ci]) ci
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
              return $ Right [GainTempStats (Stats t t)]
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
          ( return $
              Right
                [ GainTempStats (Stats 0 2),
                  GainTaunt
                ]
          )
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
moltenRock = Card MoltenRock (Stats 3 3) Elemental [Event $ AfterPlay (\card -> tribe card == Elemental) (return $ Right [GainPermStats (Stats 0 1)])]

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
              either (return . Left) (\ci -> return $ Right [RemoveFromShop ci, GainPermStats (stats (card ci))]) toEat
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
          ( AfterPlay (\c -> tribe c == SpellTODO) (return $ Right [GainPermStats (Stats 1 1)])
          )
    ]