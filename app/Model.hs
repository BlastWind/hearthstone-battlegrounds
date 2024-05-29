module Model (CardName) where
    import Control.Monad.State.Lazy (State)
    {- 
    Design Philosophy: 
    The internals of battlegrounds can be modelled as an algebra on GameState.
    -}

    -- ##### BEGIN: Enums ##### 
    data CardName = AnnoyOTron | BackStageSecurity | CordPuller | DeckSwabbie | DeepSeaAngler | GlimGuardian | HarmlessBonehead | Manasaber | Peggy
    data Tribe = Quilboar | Pirate | Mech | Elemental | Dragon | Naga | Undead | Beast
    -- ##### END:   Enums #####

    data Card = Card { 
        cardName :: CardName, 
        tier :: TavernTier, 
        tribe :: Tribe,
        attack :: Attack, 
        health :: Health, 
        battlecry :: GameState -> GameState, 
        deathrattle :: GameState -> GameState,
        onSell :: GameState -> GameState,
        onBuy  :: GameState -> GameState,
        counter :: State Int Int -- for seafarer, malchezaar, chimera, etc.

        -- Some cards have very special effects when a tangential action occurs
        -- Examples: 
        -- Peggy buffs minion when any card is added to hand
        -- Swampstriker is buffed when murloc is player
        -- Seafarer, Malchezaar, Bazaar Dealer needs a counter for its effects
        -- In combat, chimera, skyblazer can buff minions permanently
    }

    gainGold :: Int -> (GameState -> GameState)
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
    newtype UpgradeCost = UpgradeCost Int
    newtype TavernTier = TavernTier Int
    newtype Gold = Gold Int
    newtype Hand = Hand [Card]
    newtype Shop = Shop [Card]
    newtype Board = Board [Card]
    newtype PlayerHP = PlayerHP Int


    data PlayerAction = Buy | Sell | Reroll | Freeze | Upgrade

    data GameState = GameState { 
        upgradeCost :: UpgradeCost, 
        tavernTier :: TavernTier, 
        playerHP :: PlayerHP, 
        hand :: Hand, 
        shop :: Shop, 
        board :: Board, 
        gold :: Gold 
    }