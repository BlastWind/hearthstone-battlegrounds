module Controller (module Controller) where

import Card (bigDumbo)
import Control.Lens
import Control.Monad.Random (MonadRandom (getRandom))
import Data.Map hiding (foldl, map)
import Data.Maybe (fromJust)
import Logic (execAction, isGameOver, validateAction)
import Model (Action (..), CardInstance (CardInstance), GameState (..), Phase (HeroSelect), PlayerState (..), playerStates)
import Text.Parsec hiding (Error)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import View (render)
import Control.Monad.IO.Class (liftIO)
import qualified Data.Map as Map

-- START: Functions for ingesting terminal input as Action --
-- Examples:
-- b 1    -> Buy 1
-- buy 1  -> Buy 1
-- s 0    -> Sell 0
-- sell 1 -> Sell 1
-- h      -> Help
-- help   -> Help
interp :: String -> Either String Action
interp s = case parse actionParser "" s of
  Left _ -> Left "Unrecognized or incorrectly formatted command. Enter h for command list."
  Right a -> Right a

-- Parse the full input string
actionParser :: Parser Action
actionParser = do
  cmd <- choice $ map (try . string) ["buy", "b", "sell", "s", "help", "h", "endturn", "e", "play", "p"]
  spaces
  action <- actionArgumentParser cmd
  eof
  return action

-- Dispatch argument parser depending on command
actionArgumentParser :: String -> Parser Action
actionArgumentParser cmd
  | cmd `elem` ["buy", "b"] = buyArgParser
  | cmd `elem` ["sell", "s"] = sellArgParser
  | cmd `elem` ["play", "p"] = playArgParser
  | cmd `elem` ["help", "h"] = return Help
  | cmd `elem` ["endturn", "e"] = return EndTurn
  | otherwise = error "Unexpected path: actionArgumentParser should only run if it matched a command."

buyArgParser :: Parser Action
buyArgParser = do
  indStr <- many1 digit
  case readMaybe indStr of
    Just ind -> return $ Buy ind
    Nothing -> fail "Buy's argument should be a valid number."

sellArgParser :: Parser Action
sellArgParser = do
  indStr <- many1 digit
  case readMaybe indStr of
    Just ind -> return $ Sell ind
    Nothing -> fail "Sell's argument should be a valid number."

playArgParser :: Parser Action
playArgParser = do
  indStr <- many1 digit
  case readMaybe indStr of
    Just ind -> return $ Play ind
    Nothing -> fail "Play's argument should be a valid number."
-- END --

initGameState :: (MonadRandom m) => m GameState
initGameState = do
  tutorialAIGameState <- tutorialAI
  return $ GameState {_playerStates = fromList [("player", defPlayerState), ("tutorialAI", tutorialAIGameState)], turn = 0}

tutorialAI :: (MonadRandom m) => m PlayerState
tutorialAI = do
  uuid <- getRandom
  return $ defPlayerState {board = [CardInstance uuid bigDumbo], hp = 5}

defPlayerState :: PlayerState
defPlayerState =
  PlayerState
    { tier = 1,
      maxGold = 3,
      curGold = 3,
      tierUpCost = 5,
      rerollCost = 1,
      shop = [],
      board = [],
      hand = [],
      phase = HeroSelect,
      frozen = False,
      hp = 20,
      armor = 0,
      alive = True,
      opponentInformation = empty
    }

runGame :: IO ()
runGame = do
  gs <- initGameState
  initialMainPlayerState <- execAction StartGame ((gs ^. playerStates) Map.! "player")
  let gs' = gs & playerStates . ix "player" .~ initialMainPlayerState
  _ <- loop gs'
  putStrLn "Game Finished."
  where
    -- Repeat Recruit and Combat until game over
    loop gs =
      if isGameOver gs
        then do
          return gs
        else do
          let mainPlayerState = fromJust $ Data.Map.lookup "player" (_playerStates gs)
          putStrLn $ render mainPlayerState
          input <- getLine
          case interp input >>= (`validateAction` mainPlayerState) of
            Left err -> putStrLn err >> loop gs
            Right action' -> do
              newPlayerState <- liftIO $ execAction action' mainPlayerState
              loop (gs & playerStates . at "player" ?~ newPlayerState)

