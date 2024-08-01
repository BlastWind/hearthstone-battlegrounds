-- -- Logic: Handles game logic, executing user commands
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE ParallelListComp #-}

module Logic (module Logic) where

import Card (pool)
import Control.Monad.Random
import Model
import View ( helpMenu )
import Utils

-- START: Functions interfacing with PlayerAction. --

-- Can action occur in given PlayerState? Yes then thread the same action through. False then error message.
validateCommand :: Command -> GameState -> Player -> Either String Command
validateCommand (Buy ind) gs p
  | shopSize == 0 = Left "Your shop is empty."
  | ind < shopSize = return (Buy ind)
  | shopSize == 1 = Left "This hand only permits shop 0"
  | otherwise = Left $ "This shop only permits buy 0 to buy " ++ show (shopSize - 1)
  where
    ps = selectPlayer p gs
    shopSize = length ps.shop
validateCommand (Sell ind) gs p
  | boardSize == 0 = Left "You have no board."
  | ind < boardSize = return (Sell ind)
  | boardSize == 1 = Left "This hand only permits sell 0"
  | otherwise = Left $ "This board only permits sell 0 to sell " ++ show (boardSize - 1)
  where
    ps = selectPlayer p gs
    boardSize = length ps.board
validateCommand (Play ind) gs p
  | handSize == 0 = Left "You have an empty hand."
  | ind < handSize = return (Play ind)
  | handSize == 1 = Left "This hand only permits play 0"
  | otherwise = Left $ "This hand only permits play 0 to play " ++ show (handSize - 1)
  where
    ps = selectPlayer p gs
    handSize = length ps.hand

validateCommand EndTurn _ _ = return EndTurn
validateCommand Help _ _ = return Help



execCommand :: (MonadIO m, MonadRandom m) => Command -> GameState -> Player -> m GameState
execCommand (Buy ind) gs p = return $ updatePlayer p (buy ind (selectPlayer p gs)) gs

execCommand (Sell ind) gs p = return $ updatePlayer p (sell ind (selectPlayer p gs)) gs
execCommand (Play ind) gs p = return $ updatePlayer p (play ind (selectPlayer p gs)) gs
execCommand Help gs _ = liftIO (putStrLn helpMenu) >> return gs
execCommand EndTurn gs _ = enter Combat gs
-- END --

-- START: Game Transition Functions --
--   Check if only one player alive
isGameOver :: GameState -> Bool
isGameOver gs = gs.playerState.alive /= gs.aiState.alive

--   Performed when we first transition to a new game phase.
enter :: (MonadRandom m) => Phase -> GameState -> m GameState
enter Recruit gs = do
  newPlayerState <- replenish gs.playerState
  newAIState     <- replenish gs.aiState
  return gs
    { 
      playerState = newPlayerState,
      aiState = newAIState
    }
  where
    replenish ps = do
      newShop <- randomShop ps.tier
      return $ ps
        { 
          phase = Recruit,
          maxGold = ps.maxGold + 1,
          curGold = ps.maxGold + 1,
          frozen = False,
          shop = if ps.frozen then ps.shop else newShop
        }
enter _ _ = error "Other phases should not be enterable"

-- END --

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
    availableCards = filter (\card -> card._cardTier <= t) pool

sampleNFromList :: (MonadRandom m) => Int -> [a] -> m [a]
sampleNFromList _ [] = return []
sampleNFromList n xs = replicateM n sample
  where
    sample = do
      i <- getRandomR (0, length xs - 1)
      return $ xs !! i

-- END --

-- START: Functions that Actions map to --
play :: Index -> PlayerState -> PlayerState
play ind ps = ps {board = ps.board ++ [findCard ind ps.hand], hand = remove ind ps.hand}

buy :: Index -> PlayerState -> PlayerState
buy ind ps =
  let cardInstance = findCard ind ps.shop
      card = cardInstance._card
      cost = card._baseCost
      moneyLeft = ps.curGold
   in if cost > moneyLeft
        then
          error "Logic error: Attempted buying without enough money."
        else ps {curGold = moneyLeft - cost, shop = remove ind ps.shop, hand = ps.hand ++ [cardInstance]}

sell :: Index -> PlayerState -> PlayerState
sell ind ps = ps {curGold = ps.curGold + 1, board = remove ind ps.board}

roll :: (MonadRandom m) => PlayerState -> m PlayerState
roll ps = do
  newShop <- randomShop ps.tier
  return $ ps {curGold = ps.curGold - 1, shop = newShop}

tierUp :: PlayerState -> PlayerState
tierUp ps = ps {curGold = ps.curGold - ps.tierUpCost, tier = ps.tier + 1}

-- END --
