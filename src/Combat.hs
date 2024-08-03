{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE ScopedTypeVariables #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
module Combat where

import Control.Monad.Random
import Data.Record.Overloading
import Debug.Trace (trace)
import Model
import Utils (selectPlayer, updatePlayer)

-- New type for Attacker
type Attacker = Contestant

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
simulateCombat initialState = do
  attacker <- initialAttacker initialState
  go attacker initialState [initialState] -- initial board is part of state
  where
    go :: (MonadRandom m) => Attacker -> (Board, Board) -> CombatHistory -> m (CombatHistory, (Board, Board))
    go attacker state history = do
      let state' = both (filter (\ci -> ci.card.health > 0)) state
      if combatEnded state'
        then return (reverse history, state')
        else do
          newState <- performAttack attacker state'
          go (alternate attacker) newState (newState : history)

both :: (a -> b) -> (a, a) -> (b, b)
both f (a, a') = (f a, f a')

alternate :: Contestant -> Contestant
alternate One = Two
alternate Two = One

initialAttacker :: (MonadRandom m) => (Board, Board) -> m Attacker
initialAttacker (board1, board2)
  | length board1 > length board2 = return One
  | length board2 > length board1 = return Two
  | otherwise = do
      r <- getRandomR (0, 1) :: (MonadRandom m) => m Int
      return $ if r == 0 then One else Two

setAt :: Int -> a -> [a] -> [a]
setAt i x xs = take i xs ++ [x] ++ drop (i + 1) xs

performAttack :: (MonadRandom m) => Attacker -> (Board, Board) -> m (Board, Board)
performAttack attackerP (board1, board2) = do
  let (attackingBoard, defendingBoard) = case attackerP of
        One -> (board1, board2)
        Two -> (board2, board1)
  attackerIndex <- getRandomR (0, length attackingBoard - 1)
  defenderIndex <- getRandomR (0, length defendingBoard - 1)
  let attacker = attackingBoard !! attackerIndex
      defender = defendingBoard !! defenderIndex
      (attacker', defender') = trade (attacker, defender)
      attackingBoard' = setAt attackerIndex attacker' attackingBoard
      defendingBoard' = setAt defenderIndex defender' defendingBoard
  return $ case attackerP of
    One -> (attackingBoard', defendingBoard')
    Two -> (defendingBoard', attackingBoard')

trade :: (CardInstance, CardInstance) -> (CardInstance, CardInstance)
trade (attacker, defender) =
  ( attacker {card.health = attacker.card.health - defender.card.attack},
    defender {card.health = defender.card.health - attacker.card.attack}
  )

combatEnded :: (Board, Board) -> Bool
combatEnded (board1, board2) = null board1 || null board2

calculateResult :: (Board, Board) -> CombatResult
calculateResult (board1, board2)
  | not (null board1) && null board2 = Loss Two (sum $ map (\ci -> ci.card.cardTier) board1)
  | null board1 && not (null board2) = Loss One (sum $ map (\ci -> ci.card.cardTier) board2)
  | otherwise = Tie
