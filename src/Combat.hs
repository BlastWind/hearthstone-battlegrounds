{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeOperators #-}

module Combat where

import Control.Lens hiding (Index)
import Data.Bifunctor (Bifunctor (second))
import Data.List (findIndex, mapAccumL)
import Data.Map ((!))
import Data.Maybe (fromJust)
import Debug.Trace (trace)
import Effect (RNG, getRandomR)
import Effectful (Eff, (:>))
import Logic (genId)
import Model hiding (turn)
import Effectful.State.Static.Local

dealDmg :: Int -> (Health, Armor) -> (Health, Armor)
dealDmg n (hp, armor) = (hp - hpDmg, armor - armorDmg)
  where
    armorDmg = min n armor
    hpDmg = n - armorDmg

-- `fight` simulates the combat
fight :: (State GameState :> es, RNG :> es) => (PlayerId, PlayerId) -> Eff es CombatSimulation
fight (p1Id, p2Id) = do
  gs <- get
  let p1State = (gs ^. playerMap) ! p1Id
  let p2State = (gs ^. playerMap) ! p2Id
  sequence <- simulateCombat (p1Id, p2Id)
  let result = calculateResult (last sequence)
  let sim = CombatSimulation [] sequence result
  case result of
    Tie -> return sim
    Loss fighter dmg -> do
      let (loserId, loserState) = case fighter of
            One -> (p1Id, p1State)
            Two -> (p2Id, p2State)
          (hp', armor') = dealDmg dmg (loserState ^. hp, loserState ^. armor)
          loserState' =
            loserState
              & hp .~ hp'
              & armor .~ armor'
              & alive .~ (hp' > 0)
      put $ gs & playerMap . ix loserId .~ loserState'
      return sim

simulateCombat :: (State GameState :> es, RNG :> es) => (PlayerId, PlayerId) -> Eff es CombatHistory
simulateCombat (p1Id, p2Id) = do
  gs <- get
  let p1State = (gs ^. playerMap) ! p1Id
  let p2State = (gs ^. playerMap) ! p2Id
  initialAttacker <- initAttacker (p1State ^. board) (p2State ^. board)

  go
    (CombatState initialAttacker (FighterState p1State 0) (FighterState p2State 0) (gs ^. config))
    [(p1State ^. board, p2State ^. board)] -- initial board is part of state
  where
    go :: (RNG :> es) => CombatState -> CombatHistory -> Eff es CombatHistory
    go combatState history = do
      if combatEnded combatState
        then return history
        else do
          let defendingBoard = case combatState ^. attacker of
                One -> combatState ^. two . fplayerState . board
                Two -> combatState ^. one . fplayerState . board
          defenderIndex <- getRandomR (0, length defendingBoard - 1)
          let (combatState', newHistorySlices) = turn defenderIndex combatState
          go combatState' (history ++ newHistorySlices)

    combatEnded :: CombatState -> Bool
    combatEnded cs =
      null (cs ^. one . fplayerState . board) || null (cs ^. two . fplayerState . board)

turn ::
  DefenderIndex -> -- Since the caller of `turn` specifies the `di`, testing single turns is easy.
  CombatState ->
  (CombatState, CombatHistory)
turn di cs = (cs''', history)
  where
    (attackingState, _) = case cs ^. attacker of
      One -> (cs ^. one, cs ^. two)
      Two -> (cs ^. two, cs ^. one)
    cs' = trade (attackingState ^. nextAttackIndex) di cs
    (cs'', snapshots) = handleDeaths cs' -- `handleDeath` does not clean the battleground (clear deaths)
    cs''' =
      cs''
        & attacker .~ alternate (cs'' ^. attacker)
        & one . fplayerState . board .~ clearDeath (cs'' ^. one . fplayerState . board)
        & two . fplayerState . board .~ clearDeath (cs'' ^. two . fplayerState . board)
    history = map extractBoards [cs, cs'] ++ [extractBoards cs'' | not (null snapshots)] ++ [extractBoards cs''']

handleDeaths :: CombatState -> (CombatState, CombatHistory)
handleDeaths cs =
  if null (prepareDeathrattles cs)
    then (cs', [])
    else second (histories ++) (handleDeaths cs') -- keeping handling deaths if they come!
  where
    (cs', states) = mapAccumL (\cs' (fighter, id, eff) -> (interpCombatEffect (CombatEffectContext cs' fighter id) eff, cs')) cs (prepareDeathrattles cs)
    histories = map extractBoards (tail states) -- be rid of the head, which is the original `cs`
    prepareDeathrattles :: CombatState -> [(Fighter, MinionID, CardEffect)]
    prepareDeathrattles cs = [] -- TODO: implement

extractBoards :: CombatState -> (Board, Board)
extractBoards cs = (cs ^. one . fplayerState . board, cs ^. two . fplayerState . board)

interpCombatEffect :: CombatEffectContext -> CardEffect -> CombatState
interpCombatEffect (CombatEffectContext cs fighter minionId) (Summon (SpecificCard card)) = case fighter of
  One -> cs & one .~ fs'
  Two -> cs & two .~ fs'
  where
    fs = case fighter of
      One -> cs ^. one
      Two -> cs ^. two
    aliveCount = countAlive (fs ^. fplayerState . board)
    summonerInd = dIndex minionId (fs ^. fplayerState . board) -- Summoner is the one who issued the summoning
    fs'
      | aliveCount < 7 =
          fs
            & fplayerState . board .~ insertAt (summonerInd + 1) (CardInstance card id) (fs ^. fplayerState . board)
            & fplayerState . idGen .~ idGen'
      | otherwise = fs
      where
        (idGen', id) = genId (fs ^. fplayerState . idGen)
interpCombatEffect _ cf = error $ "Effect `" ++ show cf ++ "` is not yet implemented"

countAlive :: Board -> Int
countAlive = undefined

-- deterministically find a minion's index and its index through its id
dIndex :: MinionID -> Board -> Index
dIndex mId = fromJust . findIndex (\ci -> ci ^. Model.id == mId)

clearDeath :: Board -> Board
clearDeath = filter (\ci -> ci ^. card . health > 0)

-- A single attack, only the involved minions are updated. Cleave logic is handled here.
trade :: AttackerIndex -> DefenderIndex -> CombatState -> CombatState
trade ai di cs = cs'
  where
    (attackingBoard, defendingBoard) = case cs ^. attacker of
      One -> (cs ^. one . fplayerState . board, cs ^. two . fplayerState . board)
      Two -> (cs ^. two . fplayerState . board, cs ^. one . fplayerState . board)

    (attackingMinion, defendingMinion) = (attackingBoard !! ai, defendingBoard !! di)
    (attackingMinion', defendingMinion') = dmgOther (attackingMinion, defendingMinion)

    cs' = case cs ^. attacker of
      One ->
        cs
          & one . fplayerState . board .~ attackingBoard
          & two . fplayerState . board .~ defendingBoard
      Two ->
        cs
          & one . fplayerState . board .~ defendingBoard
          & two . fplayerState . board .~ attackingBoard

    dmgOther :: (CardInstance, CardInstance) -> (CardInstance, CardInstance)
    dmgOther (attacker, defender) =
      ( attacker & card . health .~ attacker ^. card . health - defender ^. card . attack,
        defender & card . health .~ defender ^. card . health - attacker ^. card . attack
      )

alternate :: Fighter -> Fighter
alternate One = Two
alternate Two = One

initAttacker :: (RNG :> es) => Board -> Board -> Eff es Fighter
initAttacker board1 board2
  | length board1 > length board2 = return One
  | length board2 > length board1 = return Two
  | otherwise = do
      r <- getRandomR (0, 1)
      return $ if r == 0 then One else Two

calculateResult :: (Board, Board) -> CombatResult
calculateResult (board1, board2)
  | not (null board1) && null board2 = Loss Two (sum $ map (\ci -> ci ^. card . cardTier) board1)
  | null board1 && not (null board2) = Loss One (sum $ map (\ci -> ci ^. card . cardTier) board2)
  | otherwise = Tie

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

insertAt :: Int -> a -> [a] -> [a]
insertAt i x xs = take i xs ++ [x] ++ drop i xs