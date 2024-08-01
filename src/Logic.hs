-- -- Logic: Handles game logic, executing user commands
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ParallelListComp #-}

module Logic (module Logic) where

import Card (pool)
import Control.Monad.Random
import Data.Functor ((<&>))
import Model
import Utils
import View (helpMenu)

execCommand :: (MonadIO m, MonadRandom m) => Command -> GameState -> Player -> m (Either String GameState)
execCommand (Buy ind) gs p = return $ buy ind (selectPlayer p gs) >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand (Sell ind) gs p = return $ sell ind (selectPlayer p gs) >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand (Play ind) gs p = return $ play ind gs p >>= (\ps' -> Right $ updatePlayer p ps' gs)
execCommand Help gs _ = liftIO (putStrLn helpMenu) >> pure (Right gs)
execCommand EndTurn gs _ = enter Combat gs <&> Right
execCommand Roll gs p = do
  ps' <- roll $ selectPlayer p gs
  return $ liftM2 (updatePlayer p) ps' (return gs)
execCommand _ gs _ = return $ Right gs

-- Game over if exactly one player is alive
isGameOver :: GameState -> Bool
isGameOver gs = gs.playerState.alive /= gs.aiState.alive

-- Performed when we first transition to a new game phase.
enter :: (MonadRandom m) => Phase -> GameState -> m GameState
enter Recruit gs = do
  newPlayerState <- replenish gs.playerState
  newAIState <- replenish gs.aiState
  return
    gs
      { playerState = newPlayerState,
        aiState = newAIState
      }
  where
    replenish ps = do
      newShop <- randomShop ps.tier
      return $
        ps
          { phase = Recruit,
            maxGold = ps.maxGold + 1,
            curGold = ps.maxGold + 1,
            frozen = False,
            shop = if ps.frozen then ps.shop else newShop
          }
enter _ _ = error "Other phases should not be enterable"

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
canTierUp ps = ps.curGold >= ps.tierUpCost

randomShop :: (MonadRandom m) => TavernTier -> m [CardInstance]
randomShop t = do
  shopCards <- sampleNFromList t availableCards
  ids <- replicateM t getRandom
  return [CardInstance uuid c | c <- shopCards | uuid <- ids]
  where
    availableCards :: [Card]
    availableCards = filter (\c -> c.cardTier <= t) pool

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
  | ind < 0 || ind >= length ps.hand || ind >= gs.config.maxBoardSize = Left "Out of bounds."
  | otherwise = Right ps {board = ps.board ++ [findCard ind ps.hand], hand = remove ind ps.hand}
  where
    ps = selectPlayer p gs

buy :: Index -> PlayerState -> Either String PlayerState
buy ind ps
  | shopSize == 0 = Left "Cannot buy. Your shop is empty."
  | ind < 0 || ind >= shopSize = Left "Out of bounds."
  | cost > moneyLeft =
      Left "Attempted buying without enough money."
  | otherwise =
      Right ps {curGold = moneyLeft - cost, shop = remove ind ps.shop, hand = ps.hand ++ [cardInstance]}
  where
    cardInstance = findCard ind ps.shop
    cost = cardInstance.card.baseCost
    moneyLeft = ps.curGold
    shopSize = length ps.shop

sell :: Index -> PlayerState -> Either String PlayerState
sell ind ps 
  | ind < 0 || ind >= length ps.board = Left "Out of bounds." 
  | otherwise = Right ps {curGold = ps.curGold + 1, board = remove ind ps.board}

roll :: (MonadRandom m) => PlayerState -> m (Either String PlayerState)
roll ps =
  if ps.curGold < ps.rerollCost
    then return $ Left "Attempted rollings without enough money"
    else do
      newShop <- randomShop ps.tier
      return $ Right $ ps {curGold = ps.curGold - 1, shop = newShop}

-- tierUp :: PlayerState -> PlayerState
-- tierUp ps = ps {curGold = ps.curGold - ps.tierUpCost, tier = ps.tier + 1}

-- END --
