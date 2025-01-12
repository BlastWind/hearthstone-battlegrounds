{-# LANGUAGE FlexibleContexts #-}
-- Logic: Handles recruit phase logic, executing user commands
-- TODO: Modularize out the recruit logic, since Combat.hs is already separate.
{-# LANGUAGE ParallelListComp #-}
{-# LANGUAGE TypeOperators #-}

module Logic (module Logic) where

import Card (pool)
import Control.Lens (At (at), Ixed (ix), traversed, (&), (.~), (<&>), (^.))
import Control.Monad (replicateM)
import Data.List (mapAccumL)
import Data.Map (elems, toList, (!))
import Effect (RNG, getRandomR)
import Effectful (Eff, IOE, MonadIO (liftIO), type (:>))
import Effectful.State.Static.Local
import Model
import View (helpMenu)

type ErrorString = String

execCommand :: (IOE :> es, RNG :> es, State GameState :> es) => Command -> PlayerId -> Eff es (Maybe ErrorString)
execCommand (Buy ind) pId = buy ind pId
execCommand (Sell ind) pId = sell ind pId
execCommand (Play ind) pId = play ind pId
execCommand Help _ = liftIO (putStrLn helpMenu) >> return Nothing
execCommand EndTurn _ = endturn
execCommand Roll pId = roll pId
execCommand TierUp pId = tierUp pId
execCommand Freeze pId = freeze pId
execCommand Concede pId = concede pId

-- Game over if exactly one player is alive
isGameOver :: GameState -> Bool
isGameOver gs = length (filter (^. alive) (elems $ gs ^. playerMap)) <= 1

-- Performed when we first transition to a new game phase.
replenish :: (RNG :> es) => PlayerState -> Eff es PlayerState
replenish ps = do
  (idGen', newShop) <- randomShop (ps ^. idGen) (ps ^. tier)
  return
    ps
      { _phase = Recruit,
        _maxGold = ps ^. maxGold + 1,
        _curGold = ps ^. maxGold + 1,
        _frozen = False,
        _shop = if ps ^. frozen then ps ^. shop else newShop,
        _idGen = idGen'
      }

-- START: Utility Methods for PlayerAction Functions --
-- Determinisitc functions should only be used when the usage permits only the happy path
deterministicLookup :: (Eq a) => a -> [(a, b)] -> b
deterministicLookup a xs =
  case lookup a xs of
    Nothing -> error "Unexpected path: deterministicLookup should always find."
    Just c -> c

findCard :: Index -> [CardInstance] -> CardInstance
findCard ind instances = instances !! ind

remove :: Int -> [a] -> [a]
remove _ [] = []
remove n xs | n < 0 = xs  -- Handle negative indices
remove n xs | n >= length xs = xs  -- Handle out of bounds
remove n xs = take n xs ++ drop (n + 1) xs

canTierUp :: PlayerState -> Bool
canTierUp ps = ps ^. curGold >= ps ^. tierUpCost

genId :: IdGen -> (IdGen, MinionID)
genId gen = (IdGen {_unIdGen = newId + 1}, newId)
  where
    newId = _unIdGen gen

genIds :: Int -> IdGen -> (IdGen, [MinionID])
genIds n gen = mapAccumL (\gen _ -> genId gen) gen [1 .. n]

randomShop :: (RNG :> es) => IdGen -> TavernTier -> Eff es (IdGen, [CardInstance])
randomShop gen t = do
  shopCards <- sampleNFromList minionsInShop availableCards
  let (gen', ids) = genIds minionsInShop gen
  return (gen', [CardInstance c id | c <- shopCards | id <- ids])
  where
    minionsInShop = case t of
      1 -> 3
      2 -> 4
      3 -> 4
      4 -> 5
      5 -> 5
      6 -> 6
      _ -> 6
    availableCards :: [Card]
    availableCards = filter (\c -> c ^. cardTier <= t) pool

sampleNFromList :: (RNG :> es) => Int -> [a] -> Eff es [a]
sampleNFromList _ [] = return []
sampleNFromList n xs = replicateM n sample
  where
    sample = do
      i <- getRandomR (0, length xs - 1)
      return $ xs !! i

-- END --

-- START: Functions that Command maps to --
play :: (State GameState :> es) => Index -> PlayerId -> Eff es (Maybe ErrorString)
play ind pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
  if ind < 0 || ind >= length (ps ^. hand) || length (ps ^. board) >= gs ^. config . maxBoardSize
    then return $ Just "Out of bounds."
    else do
      put $ gs & playerMap . ix pId .~ ps {_board = ps ^. board ++ [findCard ind (ps ^. hand)], _hand = remove ind (ps ^. hand)}
      return Nothing

buy :: (State GameState :> es) => Index -> PlayerId -> Eff es (Maybe ErrorString)
buy ind pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
      cardInstance = findCard ind (ps ^. shop)
      cost = cardInstance ^. card . baseCost
      moneyLeft = ps ^. curGold
      shopSize = length (ps ^. shop)
      ps' = ps {_curGold = moneyLeft - cost, _shop = remove ind (ps ^. shop), _hand = ps ^. hand ++ [cardInstance]}
  case () of
    _
      | shopSize == 0 -> return $ Just "Cannot buy. Your shop is empty."
      | length (ps ^. hand) >= gs ^. config . maxHandSize -> return $ Just "Your hand is full."
      | ind < 0 || ind >= shopSize -> return $ Just "Out of bounds."
      | cost > moneyLeft -> return $ Just "Attempted buying without enough money."
      | otherwise -> do
          put $ gs & playerMap . ix pId .~ ps'
          return Nothing

sell :: (State GameState :> es) => Index -> PlayerId -> Eff es (Maybe ErrorString)
sell ind pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
  case () of
    _
      | ind < 0 || ind >= length (ps ^. board) -> return $ Just "Out of bounds."
      | otherwise -> do
          put $ gs & playerMap . ix pId .~ ps {_curGold = ps ^. curGold + 1, _board = remove ind (ps ^. board)}
          return Nothing

roll :: (State GameState :> es, RNG :> es) => PlayerId -> Eff es (Maybe ErrorString)
roll pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
  case () of
    _
      | ps ^. curGold < ps ^. rerollCost -> return $ Just "Attempted rollings without enough money"
      | otherwise -> do
          (idGen', newShop) <- randomShop (ps ^. idGen) (ps ^. tier)
          put $ gs & playerMap . ix pId .~ ps {_curGold = ps ^. curGold - 1, _shop = newShop, _idGen = idGen'}
          return Nothing

endturn :: (State GameState :> es) => Eff es (Maybe ErrorString)
endturn = do
  gs <- get
  put $ gs & playerMap . traversed . phase .~ Combat
  return Nothing

-- Cost for going to the TavernTier
baseTierUpCost :: TavernTier -> Int
baseTierUpCost t = case t of
  2 -> 5
  3 -> 7
  4 -> 8
  5 -> 11
  6 -> 10
  _ -> error "Tier Up to 7 is not possible for now. So, `baseTierUpCost` shouldn't have been queried"

tierUp :: (State GameState :> es) => PlayerId -> Eff es (Maybe ErrorString)
tierUp pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
  let oldTier = ps ^. tier
  let newTier = oldTier + 1
  case () of
    _
      | oldTier == 6 -> return $ Just "Attempted to tier up but already on Tavern 6"
      | (ps ^. curGold) < (ps ^. tierUpCost) -> return $ Just "Attempted tier up without enough money"
      | otherwise -> do
          put $ gs & playerMap . ix pId .~ ps {_curGold = ps ^. curGold - ps ^. tierUpCost, _tier = newTier, _tierUpCost = if newTier == 6 then 10000 else baseTierUpCost (newTier + 1)}
          return Nothing

-- toggle frozen
freeze :: (State GameState :> es) => PlayerId -> Eff es (Maybe ErrorString)
freeze pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
  put $ gs & playerMap . ix pId .~ ps {_frozen = not (ps ^. frozen)}
  return Nothing

-- Kill player and move their render screen to the EndScreen
concede :: (State GameState :> es) => PlayerId -> Eff es (Maybe ErrorString)
concede pId = do
  gs <- get
  let ps = (gs ^. playerMap) ! pId
  put $ gs & playerMap . ix pId .~ ps {_alive = False}
  return Nothing

-- END --
