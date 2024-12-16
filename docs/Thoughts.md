These are random development thoughts/log.

### July 31, 2024
The current repo has too much structure, I think it will be fine to do without the distinction between Terminal/better GUI.

Claude provided a good roadmap of development:
1. Work out core functionalities and have a single AI. Forget about servers for now. Implement all the logic.
2. Still one main player and a single AI, but manage them through servers. I.e., server now controls the phase timers, and AI has
to issue commands to the server, etc.
3. flush out server stuff, adding game rooms, authentications, etc.

Note, server and client will be bidirectionally. Server needs to manage phase timers and ping the client when phase changes.
So, the client needs to `forkIO` twice for a sending and receiving thread. The server can have one `fork` per client.

### Aug 1, 2024: Encapsulate custom effects in callbacks
Callbacks can be used to implement both in-game logic and game rules. For example, omu's hero power can be implemented via a callback
to `tierUp`. Conversely, maybe `tierUp` can expand the `randomShopSize` via a callback, as opposed to this being built-in. For now, 
`randomShop` is directly dependent on `TavernTier`, but maybe that can change.

### Aug 4, 2024: `simulateCombat` logic
Taming `simulateCombat`. A few additions to make
- Allow each call of `go` add multiple snapshots to the combat history. Each `go` handles all that transpires from one player's turn, which involves
    - capture initial state
	- on minion attack (capture state)
	- minions trade (capture state, dead minions are snapped because this should show in the UI)
	- clear dead minions (capture state)
	- trigger deathrattles (capture state)

- `go` updates the next attack indices. This is a simple +=1 if no deaths/summons occured. On the other hand —
	- If summon(s), handle from left to right, one by one: if summonIndex <= atkIndex then atkIndex += 1
	- If death(s), handle from left to right, one by one: if deadIndex <= atkIndex then atkIndex -= 1
	- Handle summons before deaths. This way, handling deathrattles that summon works out perfectly (consider Harmless Bonehead)

Today's goal: 
- Get rid of cardId. I forgot why those were ever needed, but they are slowing progress.
- Write tests for Harmless Bonehead, and then implement!

I think since both Onyxia's whelp and Scallywag have the "attack immediately" keyword, it warrants a `AttackImmediately` keyword.
When a minion with `AttackImmediately` is summoned, it is to perform an attack.

### Aug 5, 2024: How to write tests when randomness is invovled.
How can I write tests?

They need to fulfill two requirements:
- Every thing that could be random may be controlled
- If unspecified, choices fall back to random sourcing
- Randomness are specified exactly where they are needed.

### Aug 6, 2024: Initial steps to handling randomness
To answer 8/5 questions: Write a full DSL and interpreter for all of the card effects. 
During testing, override terms like `RandomIndex` to `SpecificIndex`.

Refactoring: The API calls for many redesign opprotunities:
1. Since `FighterState` provides `PlayerState`, no need for `combatBoard`.
2. Need to seriously think about the pros and cons of the different CombatState designs.

Today's result: Combat.hs typechecks. Claude suggests to refactor `CombatState` into a map of `{ One: one's fighterstate, Two: two's fighterstate }`

### Oct 13, 2024: Rando Thoughts
Been reading Granin's FDD book. Main question I'm having:
1. What types can be "correct by construction"?


I wasn't able to get far from this question. But, I started a `Temp.hs` file in which I started mocking things in. This has proven to be such a great exercise — it's great seeing just how compositional some effects are.


****

Questions from 8/5/2024 still persist:
- How to determine everything random in tests?
- If unspecified, choices fall back to random sourcing

Thought: 

Another question:

### Oct 14, 2024:

What if language is actually a continuation-based eDSL?

```
PickTarget (\target -> cont)
Consume Target -- Somehow utilize "GainStats" here?


```

### Oct 19, 2024:
I actually free-monadized a lot of the language!

Design Challenge:
- How to model "UpTo" and "AfterPlay" logic in one statement? 
Snail Cavalry requires this, but with both `UpTo` and `AfterPlay` being
`Functionalities`, they themselves do not compose.

Resolution:
- Split `Functionalities` into `KeywordFunctionality`, `EventFunctionality`, and `FunctionalityCombinator`.

### Oct 21, 2024:
Question: How to model Brann?

Idea 1: `AfterBattlecryTrigger`. But this would require `AfterBattlecryTrigger` to pass in the `TriggeredBattlecry`, and a new `TriggerBattlecry` state effect.

Idea 2: Reserved `BrannFunctionality`. 

Idea 3: Reserved `Trigger Battlecry Times` constructor for `Functionality`.

Idea 4: The game logic keeps an priority queue for how many times do battlecries get triggered,
it's a priority queue because the highest trigger count is used. Brann, upon entering the board,
inserts "twice" into the battlecry priority queue. On leave, it deletes *some* "Battlecry twice" node.

### Oct 26, 2024:
Problem: `retrieveAssociatedCard` is not good correctness by modeling since 
a lot of events are not associated cards.

Solution:
#1: For events, it probably makes the most sense to just pass the associated card
#2: To be fancy, we could associate `retrieveAssociatedCard` with a typeclass that
only `EventFunctionalities` get access to. That still seems too much though.

----

Idea: What if I free monadized the design sketch more and delistified the `[Functionality]` that is bound to a `Card`?

----

Idea: What if every Functionality is its own datatype? And, whether or not
it is associated with "keyword", "event", or is a "combinator" 
will be described by a `Has` or `Is` style typeclass?

--- 

Key question: Battlecry effects sometimes have a target, but this target gets
randomized if the effect is to be triggered. How to model this scenario?