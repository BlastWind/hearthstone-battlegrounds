{-# LANGUAGE DataKinds #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE TypeFamilies #-}
{-# LANGUAGE TypeOperators #-}
{-# LANGUAGE TypeApplications #-}
{-# OPTIONS_GHC -Wno-incomplete-patterns #-}

module GameInterpreter where

import Control.Monad
import DesignSketch2 hiding (GameM, GameState)
import Effectful
import Effectful.Error.Static
import Effectful.State.Static.Local
import RNGDesign (GameRNG(..))
import Effectful.Dispatch.Dynamic

-- A minimal game state that can handle PickyEater's battlecry
data GameState = GameState
  { playerHand :: [CardInstance]
  , playerBoard :: [CardInstance]
  , shop :: [CardInstance]
  , currentTier :: Int
  }

-- All effects
type GameM = Eff '[State GameState, Error EffectError, Avatar, GameRNG]
type instance DispatchOf GameRNG = Dynamic

-- Avatar handler as opposed to instance Avatar methods.
runAvatar :: (State GameState :> es, Error EffectError :> es, GameRNG :> es) => Avatar m a -> Eff es a
runAvatar = \case
  QueryTier -> get >>= pure . currentTier
  
  TargetRandomCard Shop criteria -> do
    shopCards <- gets shop
    case shopCards of
      [] -> throwError "No cards in shop"
      cards -> do
        idx <- send $ ChooseFromShop (length cards)
        pure $ cards !! idx
  
  TargetRandomCards target criteria n -> do
    cards <- case target of
      Shop -> gets shop
      Board -> gets playerBoard
      Hand -> gets playerHand
    if length cards < n
      then throwError "Not enough cards to choose from"
      else replicateM n $ do
        idx <- send $ ChooseFromShop (length cards)
        pure $ cards !! idx
  
  MakeRandomCard _ -> throwError "Not implemented"
  RetrieveAssociatedCard -> throwError "Not implemented"
  RetrieveBoard -> gets playerBoard

-- Interpret a single StateEffect
interpretEffect :: (State GameState :> es, Error EffectError :> es, GameRNG :> es) 
                => StateEffect -> Eff es ()
interpretEffect = \case
  Take card -> do
    st <- get
    let newShop = filter (\c -> instanceId c /= instanceId card) (shop st)
    let newHand = card : playerHand st
    put st { shop = newShop, playerHand = newHand }
  
  GainStats stats -> return ()
  
  _ -> throwError "Effect not implemented"

-- Run a card's battlecry effect
runBattlecry :: (State GameState :> es, Error EffectError :> es, GameRNG :> es, Avatar :> es) 
             => CardInstance -> Eff es ()
runBattlecry ci = case card ci of
  Card {functionality = fs} -> do
    let battlecries = [b | Keyword (Battlecry b) <- fs]
    forM_ battlecries $ \battlecry -> do
      effects <- battlecry
      mapM_ interpretEffect effects

-- Main game action for playing a card
playCard :: CardInstance -> GameM ()
playCard ci = do
  -- First move the card from hand to board
  st <- get
  let newHand = filter (\c -> instanceId c /= instanceId ci) (playerHand st)
  let newBoard = ci : playerBoard st
  put st { playerHand = newHand, playerBoard = newBoard }
  
  -- Then trigger its battlecry if it has one
  runBattlecry ci

-- Helper to run the game monad
-- runGame :: GameM a -> GameState -> IO (Either EffectError a)
-- runGame m initialState = 
--   runEff $ 
--     runError @EffectError $ 
--       evalState initialState $
--         runGameRNG $  -- Add GameRNG handler
--           runAvatar' $ -- Add Avatar handler
--             m

-- Example initial game state for testing
initialGameState :: GameState
initialGameState = GameState
  { playerHand = [CardInstance pickyEater 1]  -- PickyEater with ID 1
  , playerBoard = []
  , shop = [CardInstance (Card DummyCard (Stats 2 2) Demon []) 2]  -- Using DummyCard constructor
  , currentTier = 1
  } 

-- Add a dummy Avatar handler (you'll need to implement this properly)
runAvatar' :: (State GameState :> es, Error EffectError :> es, GameRNG :> es) 
           => Eff (Avatar ': es) a -> Eff es a
runAvatar' = interpret $ \_ -> undefined  -- Implement proper Avatar handling