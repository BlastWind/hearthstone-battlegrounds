-- -- Logic of the game
{-# LANGUAGE ParallelListComp #-}

module Logic (module Logic) where

import Card (allCards)
import Control.Lens ((^.))
import Control.Monad.Random
import Data.UUID
import Model

findCard :: UUID -> [CardInstance] -> CardInstance
findCard cardId instances =
  case lookup cardId ([(_cardId cardInstance, cardInstance) | cardInstance <- instances]) of
    Nothing -> error "Unexpected path: findCard should always find the target."
    Just c -> c

removeCard :: UUID -> [CardInstance] -> [CardInstance]
removeCard cardId = filter (\ci -> _cardId ci /= cardId)

-- END --

play :: UUID -> PlayerState -> PlayerState
play targetId ps = ps {board = board ps ++ [findCard targetId (hand ps)], hand = removeCard targetId (hand ps)}

buy :: UUID -> PlayerState -> PlayerState
buy targetId ps =
  let cardInstance = findCard targetId (shop ps)
      card = _card cardInstance
      cost = _baseCost card
      moneyLeft = curGold ps
   in if cost > moneyLeft
        then
          error "Logic error: Attempted buying without enough money."
        else ps {curGold = moneyLeft - cost, shop = removeCard targetId (shop ps), hand = hand ps ++ [cardInstance]}

sell :: UUID -> PlayerState -> PlayerState
sell targetId ps = ps {curGold = curGold ps + 1, board = removeCard targetId (board ps)}

roll :: (MonadRandom m) => PlayerState -> m PlayerState
roll ps = do
  newShop <- randomShop (tier ps)
  return $ ps {curGold = curGold ps - 1, shop = newShop}

canTierUp :: PlayerState -> Bool
canTierUp ps = curGold ps >= tierUpCost ps

tierUp :: PlayerState -> PlayerState
tierUp ps = ps {curGold = curGold ps - tierUpCost ps, tier = tier ps + 1}

-- Performed when we first transition to a new game phase.
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
enter Combat _ = undefined
enter _ _ = error "Other phases should not be enterable"

randomShop :: (MonadRandom m) => TavernTier -> m [CardInstance]
randomShop t = do
  shopCards <- sampleNFromList t availableCards
  ids <- replicateM t getRandom
  return [CardInstance uuid c | c <- shopCards | uuid <- ids]
  where
    availableCards :: [Card]
    availableCards = filter (\card -> card ^. cardTier <= t) allCards

sampleNFromList :: (MonadRandom m) => Int -> [a] -> m [a]
sampleNFromList _ [] = return []
sampleNFromList n xs = replicateM n sample
  where
    sample = do
      i <- getRandomR (0, length xs - 1)
      return $ xs !! i