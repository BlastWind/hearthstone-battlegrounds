{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Effect (runRNG, getRandomR, RNG) where
import Effectful
import Effectful.Dispatch.Dynamic
import System.Random
import Effectful.State.Static.Local


-- Define the RNG effect
data RNG :: Effect where
  GetRandomR :: (Int, Int) -> RNG m Int

type instance DispatchOf RNG = Dynamic

runRNG :: forall g es a. RandomGen g => g -> Eff (RNG : es) a -> Eff es a
runRNG initialGen = reinterpret (evalState initialGen) $ \_ -> \case
  GetRandomR range -> do
    g <- get @g
    let (a, g') = randomR range g
    put g'
    return a

getRandomR :: (RNG :> es) => (Int, Int) -> Eff es Int
getRandomR rang = send $ GetRandomR rang

testRandomEffect :: IO ()
testRandomEffect = do
  gen <- newStdGen
  result <- runEff . runRNG gen $ (,,) <$> getRandomR (0, 100) <*> getRandomR (0, 100) <*> getRandomR (0, 100)
  print result
  print result

