-- Controller: Handles input, game loop
{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE GADTs #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Controller (module Controller) where

import Card (bigDumbo)
import Combat (fight)
import Control.Monad.Random (MonadIO, MonadRandom (getRandom), liftIO)
import Data.Record.Overloading hiding (loop)
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace, traceM)
import GHC.Base (when)
import Logic (execCommand, isGameOver, replenish)
import Model
import System.IO (hFlush, hReady, stdin, stdout)
import Text.Parsec hiding (Error)
import Text.Parsec.String (Parser)
import Text.Pretty.Simple (pPrint, pShow)
import Text.Read (readMaybe)
import View

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

initGameState :: GameState
initGameState = GameState {playerState = defPlayerState, aiState = tutorialAI, turn = 0, config = Config {maxBoardSize = 7, maxHandSize = 10}}

tutorialAI :: PlayerState
tutorialAI = defPlayerState {board = [CardInstance bigDumbo], hp = 5}

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
      phase = Recruit
    }

runGame :: IO ()
runGame = do
  _ <- loop $ return initGameState
  putStrLn "Game Loop Completed."
  where
    -- Repeat Recruit and Combat until game over
    loop :: (MonadIO m, MonadRandom m) => m GameState -> m GameState
    loop mgs = do
      gs <- mgs
      -- Go to EndScreen if applicable
      let gs' = gs {playerState.phase = if isGameOver gs then EndScreen else gs.playerState.phase}
      -- trace (TL.unpack $ pShow gs)
      case gs'.playerState.phase of
        Recruit -> do
          replenishedPlayer <- replenish gs'.playerState
          replenishedAI <- replenish gs'.aiState
          let gs'' = gs' {playerState = replenishedPlayer, aiState = replenishedAI}
          recruitLoop gs'' >>= (loop . return)
        Combat -> do
          (gs'', sim) <- fight Player AI gs'
          liftIO $ replayCombat 1 sim
          liftIO flushInput -- ignore input entered during combat phase
          liftIO $ putStrLn "finished playing"
          loop $ return gs'' {playerState.phase = Recruit, aiState.phase = Recruit}
        EndScreen -> do
          -- Note EndScreen doesn't invoke `loop`. Game logic stops here.
          liftIO $ putStrLn $ endScreenMsg gs'
          return gs
        _ -> error "Other phases not yet implemented"

recruitLoop :: (MonadIO m, MonadRandom m) => GameState -> m GameState
recruitLoop gs
  | gs.playerState.phase == Recruit = do
      liftIO $ putStrLn $ fmtRecruit gs Player
      liftIO $ putStr "> "
      liftIO $ hFlush stdout
      input <- liftIO getLine
      result <-
        either
          (return . Left)
          (\cmd -> execCommand cmd gs Player)
          (interp input)
      case result of
        Left err -> liftIO (putStrLn err) >> recruitLoop gs
        Right gs' -> recruitLoop gs'
  | otherwise = return gs

flushInput :: IO ()
flushInput = do
  ready <- hReady stdin
  when ready $ do
    _ <- getChar -- Ignores commands entered (pressing actual enter key), but, this does not ignore partially typed, not-yet-entered text
    flushInput