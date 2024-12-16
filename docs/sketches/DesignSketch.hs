-- Temp file. Place for some all-in-one mocks
{-# LANGUAGE DeriveFunctor #-}

module DesignSketch (module DesignSketch) where

import Control.Monad.Free

data CardFilterCriterion = MaxTier Int | Tribe Tribe | IsMinion | NotSelf

data RandomTarget = Hand | Shop | Board

data InjectAvatarMethod next
  = QueryTier (Int -> next)
  | MakeRandomCard [CardFilterCriterion] (CardInstance -> next)
  | TargetRandomCard RandomTarget [CardFilterCriterion] (Either EffectError CardInstance -> next)
  | TargetRandomCards RandomTarget [CardFilterCriterion] Int (Either EffectError [CardInstance] -> next)
  | RetrieveAssociatedCard (CardInstance -> next)
  | RetrieveBoard ([CardInstance] -> next)
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

retrieveAssociatedCard :: InjectAvatar CardInstance
retrieveAssociatedCard = liftF $ RetrieveAssociatedCard id

retrieveBoard :: InjectAvatar [CardInstance]
retrieveBoard = liftF $ RetrieveBoard id

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
  = -- Self events: Detectable by "inspecting" the card itself
    OnAttack (InjectAvatar (Either EffectError [StateEffect]))
  | OnDamaged (InjectAvatar (Either EffectError [StateEffect]))
  | OnKill (InjectAvatar (Either EffectError [StateEffect]))
  | OnSell (InjectAvatar (Either EffectError [StateEffect]))
  | -- Global events: Detectable by listening onto some other more "global" events
    AfterPlay (InjectAvatar (Either EffectError [StateEffect]))
  | AfterSummon (InjectAvatar (Either EffectError [StateEffect]))
  | AfterBattlecryTrigger (InjectAvatar (Either EffectError [StateEffect])) -- due to cards like Rylak, Dreamer's Embrace, this needs its own event.
  | -- Every `count` times `counterType` happens, run effects. `counterType` are more global events.
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
  | RecruitATrainee
  | BlazingSkyfin
  | AncestralAutomaton
  | BrannBronzebeard
  deriving (Eq)

data Card = Card
  { cardName :: CardName,
    stats :: Stats,
    tribe :: Tribe,
    functionality :: [Functionality]
  }

data CardInstance = CardInstance {card :: Card, instanceId :: Int}

instance Eq CardInstance where
  a == b = instanceId a == instanceId b

glimGuardian :: Card
glimGuardian = Card GlimGuardian (Stats 1 4) Dragon [Event $ OnAttack (return $ Right [GainStats (Stats 2 1)])]

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
              return $ Right [GainStats (Stats t t)]
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
                  GainTempTaunt
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
moltenRock =
  Card
    MoltenRock
    (Stats 3 3)
    Elemental
    [ Event $
        AfterPlay
          ( do
              c <- retrieveAssociatedCard
              return $ Right [GainStats (Stats 0 1) | (tribe . card) c == Elemental]
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
              either (return . Left) (\ci -> return $ Right [RemoveFromShop ci, GainStats (stats (card ci))]) toEat
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
                  return $ Right [GainStats (Stats 1 1) | (tribe . card) c == SpellTODO]
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
              return $ Right [AddToHand c]
          )
    ]

blazingSkyfin :: Card
blazingSkyfin =
  Card
    BlazingSkyfin
    (Stats 2 4)
    MurlocDragon
    [ Event $
        AfterBattlecryTrigger (return $ Right [GainStats (Stats 1 1)])
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
              return $ Right [GainPermStats (Stats 2 1) | cardName (card c) == AncestralAutomaton]
          )
    ]

data Free' t a
  = Pure' a -- Termination case
  | Free' -- Recursive nesting of language, store an outer monadic action that keeps another continuation action inside.
      ( t -- Algebra of the language
          (Free' t a) -- Nested language in the free form.
      )

-- brannBronzebeard :: Card
-- brannBronzebeard =
--   Card
--     BrannBronzebeard
--     (Stats 2 4)
--     Neutral
--     [ Event $
--         AfterBattlecryTrigger
--           ( do
--               c <- retrieveAssociatedCard
--               b <- retrieveBoard
--               -- Only do brann stuff if this is the first brann. Brann effects do not stack.
--               return _
--           )
--     ]
