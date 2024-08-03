-- Controller: Handles input, game loop
{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE KindSignatures         #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}
module Controller (module Controller) where

import Prelude
import Data.Record.Overloading
import Card (bigDumbo)
import Control.Monad.Random (MonadRandom (getRandom), MonadIO, liftIO)
import Logic (enter, execCommand, isGameOver)
import Model
import Text.Parsec hiding (Error)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import View (render)
import System.IO (hReady, stdin, hFlush, stdout)
import GHC.Base (when)
import Text.Pretty.Simple (pPrint, pShow)
import Debug.Trace (trace, traceM)
import qualified Data.Text.Lazy as TL
import Combat (fight)

-- START: Functions for ingesting terminal input as PlayerAction --
-- Examples:
-- b 1    -> Buy 1
-- buy 1  -> Buy 1
-- s 0    -> Sell 0
-- sell 1 -> Sell 1
-- h      -> Help
-- help   -> Help
interp :: String -> Either String Command
interp s = case parse actionParser "" s of
  Left _ -> Left "Unrecognized or incorrectly formatted command. Enter h for command list."
  Right a -> Right a

-- Parse the full input string
actionParser :: Parser Command
actionParser = do
  cmd <- choice $ map (try . string) ["buy", "b", "sell", "s", "play", "p", "roll", "r", "tier", "t", "freeze", "f", "endturn", "e", "help", "h", "concede"]
  spaces -- allows for 0 to many spaces, so "b0", "b 0", "b  0" ... are all valid
  action <- actionArgumentParser cmd
  eof
  return action

-- Dispatch argument parser depending on command
actionArgumentParser :: String -> Parser Command
actionArgumentParser cmd
  | cmd `elem` ["buy", "b"] = buyArgParser
  | cmd `elem` ["sell", "s"] = sellArgParser
  | cmd `elem` ["play", "p"] = playArgParser
  | cmd `elem` ["tier", "t"] = return TierUp
  | cmd `elem` ["roll", "r"] = return Roll
  | cmd `elem` ["freeze", "f"] = return Freeze
  | cmd `elem` ["endturn", "e"] = return EndTurn
  | cmd `elem` ["help", "h"] = return Help
  | cmd == "concede" = return Concede
  | otherwise = error "Unexpected path: actionArgumentParser should only run if it matched a command."

buyArgParser :: Parser Command
buyArgParser = do
  indStr <- many1 digit
  case readMaybe indStr of
    Just ind -> return $ Buy ind
    Nothing -> fail "Buy's argument should be a valid number."

sellArgParser :: Parser Command
sellArgParser = do
  indStr <- many1 digit
  case readMaybe indStr of
    Just ind -> return $ Sell ind
    Nothing -> fail "Sell's argument should be a valid number."

playArgParser :: Parser Command
playArgParser = do
  indStr <- many1 digit
  case readMaybe indStr of
    Just ind -> return $ Play ind
    Nothing -> fail "Play's argument should be a valid number."

-- END --

initGameState :: (MonadRandom m) => m GameState
initGameState = do
  tutorialAIGameState <- tutorialAI
  return $ GameState {playerState = defPlayerState, aiState = tutorialAIGameState, turn = 0, config = Config { maxBoardSize = 7, maxHandSize = 10 }}

tutorialAI :: (MonadRandom m) => m PlayerState
tutorialAI = do
  uuid <- getRandom
  return $ defPlayerState {board = [CardInstance uuid bigDumbo], hp = 5}

defPlayerState :: PlayerState
defPlayerState =
  PlayerState
    { tier = 1,
      maxGold = 300, -- By `enter`ing into the first turn, this becomes 3 as required.
      curGold = 2,
      tierUpCost = 6, -- By `enter`ing into the first turn, this becomes 5 as required.
      rerollCost = 1,
      shop = [],
      board = [],
      hand = [],
      frozen = False,
      hp = 20,
      armor = 0,
      alive = True,
      phase = HeroSelect,
      combatToReplay = CombatSimulation [] [] Tie
    }

runGame :: IO ()
runGame = do
  gs <- initGameState
  gs' <- enter Recruit Player gs
  _ <- loop (return gs')
  putStrLn "Game Loop Completed."
  where
    -- Repeat Recruit and Combat until game over
    loop :: (MonadIO m, MonadRandom m) => m GameState -> m GameState
    loop mgs = do
      gs <- mgs
      if isGameOver gs
        then do
          gs' <- enter EndScreen Player gs
          _ <- liftIO $ render gs' Player -- Render the EndScreen before exit.
          -- trace "Before loop finally returns" 
          -- $ 
          return gs'
      else 
        -- trace (TL.unpack $ pShow gs) 
        -- $ 
        case gs.playerState.phase of
        Recruit -> do
          liftIO $ render gs Player
          liftIO $ putStr "> "
          liftIO $ hFlush stdout
          input <- liftIO getLine
          result <- either -- Combine two eithers: If first action Left, propogate it. If right, execCommand and return an Either.
                      (return . Left)
                      (\cmd -> execCommand cmd gs Player)
                      (interp input)
          case result of -- Earlier, two eithers were combined together because they should run the same thing on Left.
            Left err -> liftIO (putStrLn err) >> loop (return gs)
            Right gs' -> loop (return gs')
        Combat -> do
          gs' <- fight Player AI gs
          liftIO $ render gs' Player
          liftIO flushInput
          loop $ enter Recruit Player gs'
        _ -> mgs

-- ignore input entered during combat phase
flushInput :: IO ()
flushInput = do
  ready <- hReady stdin
  when ready $ do
    _ <- getChar -- Ignores commands entered (pressing actual enter key), but, this does not ignore partially typed, not-yet-entered text
    flushInput