{-# LANGUAGE FlexibleContexts #-}
-- Controller: Handles input, game loop
{-# LANGUAGE TypeOperators #-}

module Controller (module Controller) where

import Card (bigDumbo)
import Combat (fight)
import Control.Lens
import Data.Map (fromList, (!))
import qualified Data.Text.Lazy as TL
import Debug.Trace (trace, traceM)
import Effect
import Effectful
import Effectful.Dispatch.Dynamic
import Effectful.State.Static.Local
import GHC.Base (when)
import Logic (execCommand, isGameOver, replenish)
import Model
import System.IO (hFlush, hReady, stdin, stdout)
import System.Random (newStdGen)
import Text.Parsec hiding (Error, State)
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
initGameState =
  GameState
    { _playerMap = fromList [(0, mainPlayerState), (1, tutorialAI)],
      _turn = 0,
      _config = Config {_maxBoardSize = 7, _maxHandSize = 10, _maxCombatBoardSize = 7}
    }

tutorialAI :: PlayerState
tutorialAI =
  mainPlayerState
    & board .~ [CardInstance bigDumbo 0]
    & hp .~ 5

mainPlayerState :: PlayerState
mainPlayerState =
  defPlayerState
    { _tier = 1,
      _maxGold = 300, -- By `enter`ing into the first turn, this becomes 3 as required.
      _curGold = 2,
      _tierUpCost = 6, -- By `enter`ing into the first turn, this becomes 5 as required.
      _rerollCost = 1,
      _hp = 20,
      _armor = 0,
      _alive = True,
      _phase = Recruit
    }

mainPlayerId :: Int
mainPlayerId = 0

aiPlayerId :: Int
aiPlayerId = 1

runGame :: IO ()
runGame = do
  gen <- newStdGen
  _ <- runEff . runRNG gen . evalState initGameState $ loop
  putStrLn "Game Loop Completed."
  where
    -- Repeat Recruit and Combat until game over
    loop :: (IOE :> es, RNG :> es, State GameState :> es) => Eff es ()
    loop = do
      gs <- get
      let mainPlayerPhase = (gs ^. playerMap) ! mainPlayerId ^. phase
          gs' =
            if isGameOver gs
              then gs & playerMap . ix mainPlayerId . phase .~ EndScreen
              else gs

      case mainPlayerPhase of
        Recruit -> do
          case (gs' ^. playerMap . at mainPlayerId, gs' ^. playerMap . at aiPlayerId) of
            (Just mainPlayer, Just aiPlayer) -> do
              replenishedPlayer <- replenish mainPlayer
              replenishedAI <- replenish aiPlayer
              let gs'' =
                    gs'
                      & playerMap . ix mainPlayerId .~ replenishedPlayer
                      & playerMap . ix aiPlayerId .~ replenishedAI
              put gs''
              recruitLoop
              loop
            _ -> error "Invalid player IDs in game state"
        Combat -> do
          sim <- fight (mainPlayerId, aiPlayerId)
          liftIO $ replayCombat 1 sim
          liftIO flushInput
          liftIO $ putStrLn "finished playing"
          gs'' <- get
          put $
            gs''
              & playerMap . ix mainPlayerId . phase .~ Recruit
              & playerMap . ix aiPlayerId . phase .~ Recruit
          loop
        EndScreen -> do
          liftIO $ putStrLn $ endScreenMsg gs' mainPlayerId
        _ -> error "Other phases not yet implemented"
        
recruitLoop :: (IOE :> es, RNG :> es, State GameState :> es) => Eff es ()
recruitLoop = do
  gs <- get
  case (gs ^. playerMap) ! 0 ^. phase of
    Recruit -> do
      liftIO $ putStrLn $ fmtRecruit gs 0
      liftIO $ putStr "> "
      liftIO $ hFlush stdout
      input <- liftIO getLine
      either
        (\_ -> return ())
        ( \cmd -> do
            res <- execCommand cmd 0
            maybe (return ()) (\s -> liftIO (putStrLn s) >> recruitLoop) res
        )
        (interp input)
      recruitLoop
    _ -> return ()

flushInput :: IO ()
flushInput = do
  ready <- hReady stdin
  when ready $ do
    _ <- getChar
    flushInput