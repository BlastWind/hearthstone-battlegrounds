# Design of Battleground Internals (The Model)
The most important part of modelling Battlegrounds is in realizing that it is an Algebra of `GameState`, the type that encapsulate all information of a Battlegrounds session.
Of course, sometimes, smaller, modular algebras that are part of `GameState` can be found and used. Here is what we can do with those cases:
```haskell
data GameState = GameState { 
	upgradeCost :: UpgradeCost, 
	tavernTier :: TavernTier, 
	playerHP :: PlayerHP, 
	hand :: Hand, 
	shop :: Shop, 
	board :: Board, 
	gold :: Gold 
}
update :: GameState -> GameState
updateCard :: Card -> Card
upgradeScope :: (Card -> Card) -> (GameState -> GameState)
```

### Guiding Questions
#### Modelling `Malchezaar`, `Seafarer`, `Bazaar Dealer`, `Chimera`, `Octosari`, `Felboar`, `Elise`? 

The named minions are have special effects that also have the notion of "counters" to them.

First, I write some algebraic laws:
```haskell
reroll :: GameState -> GameState
-- Law: Rolling with Malchezaar
forall (g :: GameState). 
	reroll g = todo.
```

**Design**: Cards implements `onEvent :: (GameState -> GameState, OverrideDefault)`.
For example, `reroll` calls the `onReroll` function of minions. Minions that don't have a `reroll` affect just has `onReroll = id`. `buy` calls the `onBuy` function of `Bazaar Dealer`. Minions that don't have a `onBuy` affect just has `onBuy = id`.

```haskell
-- simplified pseudocode
data Override = Override | ComposableWithDefault
malchezaar :: Card
malchezaar = Card {
	onReroll = (\prevGameState -> subtractHealth prevGameState),
	rerollComposability = Override,
	counter = State 2
}

elise :: Card
elise = Card {
	onReroll = {\prevGameState -> addGoldenMonkeyToShop and update state prevGameState},
	rerollComposability = ComposableWithDefault,
	counter = State 5
**Design asdf**
}

reroll :: GameState -> GameState
reroll (GameState board) = fold (\card -> onReroll card) board
```

Con: Cards should inform `reroll` if they have special `reroll` behavior that should completely override the default behavior (of subtracting 1 coin and refreshing shop). But special cards can override this behavior. Whether to override the default behavior
is card specific. This is a nuisance. I can technically make everything composable (no overriding) by adjusting `malchezaar`'s affect to "adding 1 gold and subtracting one health", but that's confusing.

#### Modelling `Peggy`, `Bream Counter`.
**Design 1**: Model effect as an `OnEvent` as well. In particular, every card has a `onAnotherCardEnterHand` attribute.
Con: Most cards don't need a `onAnotherCardEnterHand` attribute, it is so specific that we shouldn't add it for just two cards to work.

**Design 2**: Event-driven.

Whenever a card is added to hand, we invoke the global `onCardEnterHand` event. When `peggy` was played, it registers a listener to the `onCardEnterHand` event. 
When `BreamCounter` was bought, it registers a listener to the `onCardEnterHand` event. `GameState` has a `events :: (Map Events Listeners)`, the listeners are also `GameState -> GameState`.

Pro: The `onCardEnterHand` event will be needed anyways.

Con: We probably have to add `id` field to `Card`. We also need to handle the register and unregister code, which is something that feels less algebraic and denotational.

#### How to make triples.
**Design 1**: Event-driven. 

Whenever we add a card to hand, we want to invoke the `onCardEnterHand` event. From there, the `tripleMaker`, who is listening to it, can scan the board and hand, and maybe make a triple.


#### Modelling effect of a `Land Lubber`, `Malchezaar`, or `Elise` pair

#### Modelling minion buffs (`Echoing Roar`, `Deep Blue`, and `Undead Army`)



#### How to model discover (consider `Reef Explorer`)

#### How to handle dying on other actions?
Say we die on Malchazaar roll.

**Design**: Event-driven.
Whenever we manipulate health, we invoke the `onNewHealth` event.