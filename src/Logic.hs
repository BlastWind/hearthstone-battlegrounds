-- -- Logic of the game
{-# LANGUAGE ParallelListComp #-}

module Logic (module Logic) where

import Card (pool)
import Control.Lens ((^.))
import Control.Monad.Random
import Model
import View (helpMenu)

-- START: Functions interfacing with Action. --

-- Can action occur in given PlayerState? Yes then thread the same action through. False then error message.
validateAction :: Action -> PlayerState -> Either String Action
validateAction (Buy ind) gs
  | shopSize == 0 = Left "Your shop is empty."
  | ind < shopSize = return (Buy ind)
  | shopSize == 1 = Left "This hand only permits shop 0"
  | otherwise = Left $ "This shop only permits buy 0 to buy " ++ show (shopSize - 1)
  where
    shopSize = length (shop gs)
validateAction (Sell ind) gs
  | boardSize == 0 = Left "You have no board."
  | ind < boardSize = return (Sell ind)
  | boardSize == 1 = Left "This hand only permits sell 0"
  | otherwise = Left $ "This board only permits sell 0 to sell " ++ show (boardSize - 1)
  where
    boardSize = length (board gs)
validateAction (Play ind) gs
  | handSize == 0 = Left "You have an empty hand."
  | ind < handSize = return (Play ind)
  | handSize == 1 = Left "This hand only permits play 0"
  | otherwise = Left $ "This hand only permits play 0 to play " ++ show (handSize - 1)
  where
    handSize = length (hand gs)
validateAction StartGame ps = if phase ps == HeroSelect then return StartGame else Left "No need to start game if you're not in HeroSelect"
validateAction EndTurn ps = if phase ps == Recruit then return EndTurn else Left "Doesn't make sense to end turn if you're not in Recruit"
validateAction x _ = return x

execAction :: (MonadIO m, MonadRandom m) => Action -> PlayerState -> m PlayerState
execAction (Buy ind) gs = return $ buy ind gs
execAction (Sell ind) gs = return $ sell ind gs
execAction (Play ind) gs = return $ play ind gs
execAction StartGame gs = enter Recruit gs -- For now, StartGame skips hero select. It should go into HeroSelect in the future
execAction EndTurn gs = enter Combat gs
execAction Help gs = liftIO (putStrLn helpMenu) >> return gs

-- END --

-- START: Game Transition Functions --
--   Check if only one player alive
isGameOver :: GameState -> Bool
isGameOver gs = playersAlive == (1 :: Integer)
  where
    playersAlive = foldl (\acc ps -> if alive ps then acc + 1 else acc) 0 (_playerStates gs)

--   Performed when we first transition to a new game phase.
enter :: (MonadRandom m) => Phase -> PlayerState -> m PlayerState
enter Recruit ps = do
  newShop <- randomShop (tier ps)
  return $
    ps
      { maxGold = newGold,
        curGold = newGold,
        phase = Recruit,
        frozen = False,
        shop = if frozen ps then shop ps else newShop
      }
  where
    newGold = maxGold ps + 1
enter Combat ps = return $ ps { phase = Combat }
enter _ _ = error "Other phases should not be enterable"

-- END --

-- START: Utility Methods for Action Functions --
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
canTierUp ps = curGold ps >= tierUpCost ps

randomShop :: (MonadRandom m) => TavernTier -> m [CardInstance]
randomShop t = do
  shopCards <- sampleNFromList t availableCards
  ids <- replicateM t getRandom
  return [CardInstance uuid c | c <- shopCards | uuid <- ids]
  where
    availableCards :: [Card]
    availableCards = filter (\card -> card ^. cardTier <= t) pool

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
play ind ps = ps {board = board ps ++ [findCard ind (hand ps)], hand = remove ind (hand ps)}

buy :: Index -> PlayerState -> PlayerState
buy ind ps =
  let cardInstance = findCard ind (shop ps)
      card = _card cardInstance
      cost = _baseCost card
      moneyLeft = curGold ps
   in if cost > moneyLeft
        then
          error "Logic error: Attempted buying without enough money."
        else ps {curGold = moneyLeft - cost, shop = remove ind (shop ps), hand = hand ps ++ [cardInstance]}

sell :: Index -> PlayerState -> PlayerState
sell ind ps = ps {curGold = curGold ps + 1, board = remove ind (board ps)}

roll :: (MonadRandom m) => PlayerState -> m PlayerState
roll ps = do
  newShop <- randomShop (tier ps)
  return $ ps {curGold = curGold ps - 1, shop = newShop}

tierUp :: PlayerState -> PlayerState
tierUp ps = ps {curGold = curGold ps - tierUpCost ps, tier = tier ps + 1}

-- END --
