-- Logic: Handles recruit phase logic, executing user commands
-- TODO: Modularize out the recruit logic, since Combat.hs is already separate.
{-# LANGUAGE ParallelListComp #-}

module Logic (module Logic) where

import Card (pool)
import Control.Monad.Random
import Data.List (foldl', mapAccumL)
import Model
import Utils
import View (helpMenu)
import Control.Lens hiding (Index)

execCommand :: (MonadIO m, MonadRandom m) => Command -> GameState -> Player -> m (Either String GameState)
execCommand (Buy ind) gs p = return $ buy ind gs p >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand (Sell ind) gs p = return $ sell ind (selectPlayer p gs) >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand (Play ind) gs p = return $ play ind gs p >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand Help gs Player = liftIO (putStrLn helpMenu) >> pure (Right gs)
execCommand Help gs AI = error "Dev Error: AI shouldn't issue `Help`."
execCommand EndTurn gs Player = return $ Right gs { _playerState = (gs ^. playerState) { _phase = Combat } }
-- In tutorial mode, the single player issues the combat (on their will) and fights the AI always
-- In multiplayer, the server issues the combat (by a timer) and pairs up who to fight
execCommand EndTurn _ AI = error "Dev Error: AI really shouldn't issue `EndTurn`."
execCommand Roll gs p = do
  ps' <- roll $ selectPlayer p gs
  return $ liftM2 (updatePlayer p) ps' (return gs)
execCommand TierUp gs p = return $ tierUp (selectPlayer p gs) >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand Freeze gs p = return $ freeze (selectPlayer p gs) >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand Concede gs p = return $ concede (selectPlayer p gs) >>= (\ps' -> Right $ updatePlayer p ps' gs)

-- Game over if exactly one player is alive
isGameOver :: GameState -> Bool
isGameOver gs = gs ^. playerState . alive /= gs ^. aiState . alive

-- Performed when we first transition to a new game phase.
replenish :: (MonadRandom m) => PlayerState -> m PlayerState
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
remove 0 (_ : xs) = xs
remove n (x : xs) = x : remove (n - 1) xs

canTierUp :: PlayerState -> Bool
canTierUp ps = ps ^. curGold >= ps ^. tierUpCost

genId :: IdGen -> (IdGen, MinionID)
genId gen = (IdGen {_unIdGen = newId + 1}, newId)
  where
    newId = _unIdGen gen

genIds :: Int -> IdGen -> (IdGen, [MinionID])
genIds n gen = mapAccumL (\gen _ -> genId gen) gen [1 .. n]

randomShop :: (MonadRandom m) => IdGen -> TavernTier -> m (IdGen, [CardInstance])
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

sampleNFromList :: (MonadRandom m) => Int -> [a] -> m [a]
sampleNFromList _ [] = return []
sampleNFromList n xs = replicateM n sample
  where
    sample = do
      i <- getRandomR (0, length xs - 1)
      return $ xs !! i

-- END --

-- START: Functions that Command maps to --
play :: Index -> GameState -> Player -> Either String PlayerState
play ind gs p
  | ind < 0 || ind >= length (ps ^. hand) || length (ps ^. board) >= gs ^. config . maxBoardSize = Left "Out of bounds."
  | otherwise = Right ps {_board = ps ^. board ++ [findCard ind (ps ^. hand)], _hand = remove ind (ps ^. hand)}
  where
    ps = selectPlayer p gs

buy :: Index -> GameState -> Player -> Either String PlayerState
buy ind gs p
  | shopSize == 0 = Left "Cannot buy. Your shop is empty."
  | length (ps ^. hand) >= gs ^. config . maxHandSize = Left "Your hand is full"
  | ind < 0 || ind >= shopSize = Left "Out of bounds."
  | cost > moneyLeft =
      Left "Attempted buying without enough money."
  | otherwise =
      Right ps {_curGold = moneyLeft - cost, _shop = remove ind (ps ^. shop), _hand = ps ^. hand ++ [cardInstance]}
  where
    ps = selectPlayer p gs
    cardInstance = findCard ind (ps ^. shop)
    cost = cardInstance ^. card . baseCost
    moneyLeft = ps ^. curGold
    shopSize = length (ps ^. shop)

sell :: Index -> PlayerState -> Either String PlayerState
sell ind ps
  | ind < 0 || ind >= length (ps ^. board) = Left "Out of bounds."
  | otherwise = Right ps {_curGold = ps ^. curGold + 1, _board = remove ind (ps ^. board)}

roll :: (MonadRandom m) => PlayerState -> m (Either String PlayerState)
roll ps =
  if ps ^. curGold < ps ^. rerollCost
    then return $ Left "Attempted rollings without enough money"
    else do
      (idGen', newShop) <- randomShop (ps ^. idGen) (ps ^. tier)
      return $ Right $ ps {_curGold = ps ^. curGold - 1, _shop = newShop, _idGen = idGen'}

-- Cost for going to the TavernTier
baseTierUpCost :: TavernTier -> Int
baseTierUpCost t = case t of
  2 -> 5
  3 -> 7
  4 -> 8
  5 -> 11
  6 -> 10
  _ -> error "Tier Up to 7 is not possible for now. So, `baseTierUpCost` shouldn't have been queried"

tierUp :: PlayerState -> Either String PlayerState
tierUp ps
  | ps ^. tier == 6 = Left "Attempted to tier up but already on Tavern 6"
  | ps ^. curGold < ps ^. tierUpCost = Left "Attempted tier up without enough money"
  | otherwise = return $ ps {_curGold = ps ^. curGold - ps ^. tierUpCost, _tier = newTier, _tierUpCost = if newTier == 6 then 10000 else baseTierUpCost (newTier + 1)}
  where
    newTier = ps ^. tier + 1

-- toggle frozen
freeze :: PlayerState -> Either String PlayerState
freeze ps = return ps {_frozen = not (ps ^. frozen)}

-- Kill player and move their render screen to the EndScreen
concede :: PlayerState -> Either String PlayerState
concede ps = return ps {_alive = False}

-- END --
