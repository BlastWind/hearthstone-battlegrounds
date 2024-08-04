{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Combat where

import Control.Monad.Random
import Data.Record.Overloading
import Debug.Trace (trace)
import Model
import Utils (selectPlayer, updatePlayer)

dealDmg :: Int -> (Health, Armor) -> (Health, Armor)
dealDmg n (hp, armor) = (hp - hpDmg, armor - armorDmg)
  where
    armorDmg = min n armor
    hpDmg = n - armorDmg

-- `fight` simulates the combat
fight :: (MonadRandom m) => Player -> Player -> GameState -> m (GameState, CombatSimulation)
fight p1 p2 gs = do
  (sequence, finalState) <- simulateCombat ((selectPlayer p1 gs).board, (selectPlayer p2 gs).board)
  let result = calculateResult finalState
  let sim = CombatSimulation [] sequence result
  case result of
    Tie -> return (gs, sim)
    Loss contestant dmg ->
      let loser = case contestant of
            One -> Player
            Two -> AI
          loserState = selectPlayer loser gs
          (hp', armor') = dealDmg dmg (loserState.hp, loserState.armor)
          loserState' = loserState {hp = hp', armor = armor', alive = hp' > 0}
       in return (updatePlayer loser loserState' gs, sim)

-- For now, the algorithm is wrong but simple:
-- Players do alternate attacking, but the attacking and defending minions are both random.
simulateCombat :: (MonadRandom m) => (Board, Board) -> m (CombatHistory, (Board, Board))
simulateCombat initialState@(board1, board2) = do
  (attacker, defender) <- initOrder initialState
  let (attackingBoard, defendingBoard) = case (attacker, defender) of
        (One, _) -> (board1, board2)
        (Two, _) -> (board2, board1)
  go
    (CombatState (ContestantState attacker attackingBoard 0) (ContestantState defender defendingBoard 0))
    [initialState] -- initial board is part of state
  where
    go :: (MonadRandom m) => CombatState -> CombatHistory -> m (CombatHistory, (Board, Board))
    go combatState history = undefined
    --   -- Logically it makes sense to `clearTheDead` after `trade` and before next loop. But, we elect to `clearTheDead` right after the loop so dead minions get recorded in snapshot
    --   let combatState' = clearTheDead combatState
    --   if combatEnded combatState'
    --     then return (reverse history, state')
    --     else dow
    --       let (attackingBoard, defendingBoard) = case attacker of
    --             One -> (board1, board2)
    --             Two -> (board2, board1)
    --       attackingMinionIndex <- getRandomR (0, length attackingBoard - 1)
    --       defendingMinionIndex <- getRandomR (0, length defendingBoard - 1)
    --       let attackingMinion = attackingBoard !! attackingMinionIndex
    --           defendingMinion = defendingBoard !! defendingMinionIndex
    --           (attacker', defender') = trade (attackingMinion, defendingMinion)
    --           attackingBoard' = setAt attackingMinionIndex attacker' attackingBoard
    --           defendingBoard' = setAt defendingMinionIndex defender' defendingBoard
    --       let state'' = case attacker of
    --             One -> (attackingBoard', defendingBoard')
    --             Two -> (defendingBoard', attackingBoard')
    --       go combatState'' (state'' : history)

    -- clearTheDead :: CombatState -> CombatState
    -- clearTheDead combatState =
    --   combatState
    --     { attacker.board = filter (\ci -> ci.card.health > 0) combatState.attacker.board,
    --       defender.board = filter (\ci -> ci.card.health > 0) combatState.defender.board
    --     }

    -- combatEnded :: CombatState -> Bool
    -- combatEnded combatState = null combatState.attacker.board || null combatState.defender.board

alternate :: Contestant -> Contestant
alternate One = Two
alternate Two = One

initOrder :: (MonadRandom m) => (Board, Board) -> m (Attacker, Defender)
initOrder (board1, board2)
  | length board1 > length board2 = return (One, Two)
  | length board2 > length board1 = return (Two, One)
  | otherwise = do
      r <- getRandomR (0, 1) :: (MonadRandom m) => m Int
      return $ if r == 0 then (One, Two) else (Two, One)

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

trade :: (CardInstance, CardInstance) -> (CardInstance, CardInstance)
trade (attacker, defender) =
  ( attacker {card.health = attacker.card.health - defender.card.attack},
    defender {card.health = defender.card.health - attacker.card.attack}
  )



calculateResult :: (Board, Board) -> CombatResult
calculateResult (board1, board2)
  | not (null board1) && null board2 = Loss Two (sum $ map (\ci -> ci.card.cardTier) board1)
  | null board1 && not (null board2) = Loss One (sum $ map (\ci -> ci.card.cardTier) board2)
  | otherwise = Tie
