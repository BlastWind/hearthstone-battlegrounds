{-# LANGUAGE OverloadedRecordDot #-}

module Combat where

import Control.Monad.Random
import Data.List (sortOn)
import Data.Ord (Down (Down))
import Model
import Utils (selectPlayer)

-- New type for Attacker
type Attacker = Contestant

-- `fight` simulates the combat and logs every move and intermediate combat state.
fight :: (MonadRandom m) => Player -> Player -> GameState -> m (CombatSimulation, CombatResult, Damage)
fight p1 p2 gs = do
  (sequence, finalState) <- simulateCombat ((selectPlayer p1 gs).board, (selectPlayer p2 gs).board)
  let result = determineCombatResult finalState
  let damage = calculateDamage result finalState
  return (CombatSimulation [] sequence, result, damage)

simulateCombat :: (MonadRandom m) => (Board, Board) -> m (CombatHistory, (Board, Board))
simulateCombat state = go state []
  where
    go state history
      | combatEnded state = return (reverse history, state)
      | otherwise = do
          attacker <- chooseAttacker state
          newState <- performAttack attacker state
          go newState (state : history)

displayInOrder :: Int -> Board -> Board
displayInOrder i b = _

chooseAttacker :: (MonadRandom m) => (Board, Board) -> m Attacker
chooseAttacker (board1, board2)
  | length board1 > length board2 = return One
  | length board2 > length board1 = return Two
  | otherwise = do
      r <- getRandomR (0, 1) :: (MonadRandom m) => m Int
      return $ if r == 0 then One else Two

performAttack :: (MonadRandom m) => Attacker -> (Board, Board) -> m (Board, Board)
performAttack attacker (board1, board2) = do
  let (attackingBoard, defendingBoard) = case attacker of
        One -> (board1, board2)
        Two -> (board2, board1)
  defenderIndex <- selectRandomDefender defendingBoard
  let (newAttackingBoard, newDefendingBoard) = atk (head attackingBoard) defenderIndex (attackingBoard, defendingBoard)
  let rotatedAttackingBoard = tail newAttackingBoard ++ [head newAttackingBoard]
  return $ case attacker of
    One -> (rotatedAttackingBoard, newDefendingBoard)
    Two -> (newDefendingBoard, rotatedAttackingBoard)

selectRandomDefender :: (MonadRandom m) => Board -> m Int
selectRandomDefender board = getRandomR (0, length board - 1)

atk :: CardInstance -> Int -> (Board, Board) -> (Board, Board)
atk attacker defenderIndex (attackerBoard, defenderBoard) =
  let defender = defenderBoard !! defenderIndex
      newAttacker = attacker {card = (attacker.card) {health = max 0 (attacker.card.health - defender.card.attack)}}
      newDefender = defender {card = (defender.card) {health = max 0 (defender.card.health - attacker.card.attack)}}
      newAttackerBoard = if newAttacker.card.health > 0 then newAttacker : tail attackerBoard else tail attackerBoard
      newDefenderBoard =
        take defenderIndex defenderBoard
          ++ ([newDefender | newDefender.card.health > 0])
          ++ drop (defenderIndex + 1) defenderBoard
   in (newAttackerBoard, newDefenderBoard)

combatEnded :: (Board, Board) -> Bool
combatEnded (board1, board2) = null board1 || null board2

determineCombatResult :: (Board, Board) -> CombatResult
determineCombatResult (board1, board2)
  | not (null board1) && null board2 = Loser Two
  | null board1 && not (null board2) = Loser One
  | otherwise = Tie

calculateDamage :: CombatResult -> (Board, Board) -> Damage
calculateDamage result (board1, board2) =
  case result of
    Loser One -> sum $ map (\ci -> ci.card.cardTier) board2
    Loser Two -> sum $ map (\ci -> ci.card.cardTier) board1
    Tie -> 0
