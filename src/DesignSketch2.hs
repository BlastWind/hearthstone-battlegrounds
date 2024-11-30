{-# LANGUAGE DeriveFunctor #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}

import Control.Monad.Random
import Control.Monad.Reader
import Data.IORef

class (Monad m) => Avatar m where
  queryTier :: m Int
  makeRandom :: m Card

newtype StateEffect = Summon String

newtype Functionality = Deathrattle [StateEffect]

data Card = Card
  { cardName :: String,
    functionality :: [Functionality]
  }

-- Example conversion of one of the cards
randomSummoner :: (Avatar m) => m Card
randomSummoner = do
  c <- makeRandom
  return $
    Card
      "randomSummoner"
      [Deathrattle [Summon (cardName c)]]

data GameState = GameState {gameTier :: Int}

-- Real game instance
newtype GameM a = GameM (ReaderT GameState (RandT StdGen IO) a)
  deriving (Functor, Applicative, Monad, MonadRandom, MonadIO)

instance Avatar GameM where
  queryTier = GameM $ asks gameTier
  makeRandom = do
    tier <- queryTier
    cards <- GameM $ asks (cardsInPool . filterByTier tier)
    -- getRandomR from MonadRandom
    i <- getRandomR (0, length cards - 1)
    pure $ cards !! i

filterByTier :: Int -> GameState -> b0
filterByTier = _

cardsInPool :: b0 -> [Card]
cardsInPool = _

-- Test instance
data TestConfig = TestConfig
  { mockTier :: Int,
    mockCards :: [Card], -- Default sequence
    mockCardGen :: Int -> Card -- Or generate based on context
  }

newtype TestM a = TestM (ReaderT TestConfig IO a)
  deriving (Functor, Applicative, Monad, MonadIO)

instance Avatar TestM where
  queryTier = TestM $ asks mockTier
  makeRandom =
    TestM $
      asks mockCards >>= \case
        (c : cs) -> do
          -- Update remaining cards for next call
          liftIO $ modifyIORef' cardsRef (const cs)
          pure c
        [] -> asks mockCardGen <*> queryTier

cardsRef :: IORef [Card]
cardsRef = _

-- Example usage:
runGame :: GameM a -> GameState -> IO a
runGame (GameM m) gs = do
  gen <- newStdGen
  evalRandT (runReaderT m gs) gen

runTest :: TestM a -> TestConfig -> IO a
runTest (TestM m) = runReaderT m

-- Example test
testRandomSummoner :: IO ()
testRandomSummoner = do
  let config =
        TestConfig
          { mockTier = 2,
            mockCards = [Card "predetermined1" [], Card "predetermined2" []],
            mockCardGen = \tier -> Card ("generated" ++ show tier) []
          }

  result <- runTest (functionality randomSummoner) config
  _

-- Assert on result