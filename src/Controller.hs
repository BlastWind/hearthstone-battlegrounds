-- Controller: Handles input, game loop

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
import Control.Lens

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
initGameState = GameState {_playerState = defPlayerState, _aiState = tutorialAI, _turn = 0, _config = Config {_maxBoardSize = 7, _maxHandSize = 10}}

tutorialAI :: PlayerState
tutorialAI = mainPlayerState 
  & board .~ [CardInstance bigDumbo 0]
  & hp .~ 5

mainPlayerState :: PlayerState
mainPlayerState =
  PlayerState
    { _tier = 1,
      _maxGold = 300, -- By `enter`ing into the first turn, this becomes 3 as required.
      _curGold = 2,
      _tierUpCost = 6, -- By `enter`ing into the first turn, this becomes 5 as required.
      _rerollCost = 1,
      _shop = [],
      _board = [],
      _hand = [],
      _frozen = False,
      _hp = 20,
      _armor = 0,
      _alive = True,
      _phase = Recruit,
      _idGen = IdGen 0
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
      let gs' = gs & playerState.phase %~ \p -> if isGameOver gs then EndScreen else p
      
      case gs' ^. playerState.phase of
        Recruit -> do
          replenishedPlayer <- replenish (gs' ^. playerState)
          replenishedAI <- replenish (gs' ^. aiState)
          let gs'' = gs' & playerState .~ replenishedPlayer 
                        & aiState .~ replenishedAI
          recruitLoop gs'' >>= (loop . return)
          
        Combat -> do
          (gs'', sim) <- fight Player AI gs'
          liftIO $ replayCombat 1 sim
          liftIO flushInput
          liftIO $ putStrLn "finished playing"
          loop $ return $ gs'' & playerState.phase .~ Recruit 
                              & aiState.phase .~ Recruit
                              
        EndScreen -> do
          liftIO $ putStrLn $ endScreenMsg gs'
          return gs
          
        _ -> error "Other phases not yet implemented"

recruitLoop :: (MonadIO m, MonadRandom m) => GameState -> m GameState
recruitLoop gs
  | gs ^. playerState.phase == Recruit = do
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