{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module RNGDesign where

import Control.Monad.Identity (Identity)
import Control.Monad.State (StateT)
import DesignSketch2
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.Dispatch.Static
import Effectful.State.Static.Local
import System.Random (randomRIO)

-- import Effectful.Internal.Utils (unsafeEff_)

data PlayerState = PlayerState

data Result = Result deriving (Show)

data GameRNG :: Effect where
  ChooseFirstAttacker :: GameRNG m Int
  ChooseDefendingMinion :: Int -> GameRNG m Int
  ChooseFromShop :: Int -> GameRNG m Int

type instance DispatchOf GameRNG = Dynamic

-- Real RNG Effect Handler is implemented with System.Random
runGameRNG :: (IOE :> es) => Eff (GameRNG : es) a -> Eff es a
runGameRNG = interpret $ \_ -> \case
  ChooseFirstAttacker -> liftIO $ randomRIO (0, 1)
  ChooseDefendingMinion n -> liftIO $ randomRIO (0, n - 1)

data MockRNGSources = MockRNGSources
  { firstAttackerChoices :: [Int],
    defendingMinionChoices :: [Int]
  }

-- Mock RNG Effect Handler is implemented by taking in a concrete mock product
runGameRNGMock :: MockRNGSources -> Eff (GameRNG : es) a -> Eff es a
runGameRNGMock initial =
  reinterpret
    (evalState initial)
    ( \_ -> \case
        ChooseFirstAttacker -> do
          s <- get
          case firstAttackerChoices s of
            (x : xs) -> do
              put (s {firstAttackerChoices = xs})
              pure x
            [] -> error "No more mock values for first attacker"
        ChooseDefendingMinion _ -> do
          s <- get
          case defendingMinionChoices s of
            (x : xs) -> do
              put (s {defendingMinionChoices = xs})
              pure x
            [] -> error "No more mock values for defending minion"
    )

-- Our main fight function using the effect
fight1 :: (GameRNG :> es) => PlayerState -> PlayerState -> Eff es Result
fight1 _p1 _p2 = do
  _firstPlayer <- send ChooseFirstAttacker
  unsafeEff_ $ putStrLn $ "First player chosen: " ++ show _firstPlayer

  _defender1 <- send $ ChooseDefendingMinion 5
  unsafeEff_ $ putStrLn $ "First defender chosen: " ++ show _defender1

  _defender2 <- send $ ChooseDefendingMinion 3
  unsafeEff_ $ putStrLn $ "Second defender chosen: " ++ show _defender2

  pure Result

fightWithRealRNG :: PlayerState -> PlayerState -> IO Result
fightWithRealRNG p1 p2 =
  runEff $
    runGameRNG $
      fight1 p1 p2

-- Example usage with mock values for testing
fightWithMockRNG :: PlayerState -> PlayerState -> Result
fightWithMockRNG p1 p2 =
  runPureEff $
    runGameRNGMock MockRNGSources {firstAttackerChoices = [0, 2, 1], defendingMinionChoices = [0, 2, 1]} $
      fight1 p1 p2

interp :: KeywordFunctionality -> StateT PlayerState Identity ()
interp (Battlecry a) = return ()

