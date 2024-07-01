module Controller.Terminal (module Controller.Terminal) where

import Card (bigDumbo)
import Control.Monad.Random (MonadRandom (getRandom))
import Data.Map hiding (foldl, map)
import Model (Action (..), CardInstance (CardInstance), GameState (..), Phase (Blank), PlayerState (..))
import Text.Parsec hiding (Error)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import View.Terminal (render)
import Data.Maybe (fromJust)
import Logic (validateAction, execAction, isGameOver)

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
             Left _  -> Left "Unrecognized or incorrectly formatted command. Enter :h for command list."
             Right a -> Right a

-- Parse the full input string
actionParser :: Parser Action
actionParser = do
  cmd <- choice $ map (try . string) ["buy", "b", "sell", "s", "help", "h", "endturn", "e"]
  spaces
  action <- actionArgumentParser cmd
  eof
  return action

-- Dispatch argument parser depending on command
actionArgumentParser :: String -> Parser Action
actionArgumentParser cmd
  | cmd `elem` ["buy", "b"] = buyArgParser
  | cmd `elem` ["sell", "s"] = sellArgParser
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

-- END --


initGameState :: (MonadRandom m) => m GameState
initGameState = do
  tutorialAIGameState <- tutorialAI
  return $ GameState {playerStates = fromList [("player", defPlayerState), ("tutorialAI", tutorialAIGameState)], turn = 0}

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
      phase = Blank,
      frozen = False,
      hp = 20,
      armor = 0,
      alive = True,
      opponentInformation = empty
    }

runGame :: IO ()
runGame = do
  gs <- initGameState
  _ <- loop gs
  putStrLn "Game Finished."
  where
    loop gs =
      if isGameOver gs
        then do
          return gs
        else do
          let mainPlayerState = fromJust $ Data.Map.lookup "player" (playerStates gs)
          putStrLn $ render mainPlayerState
          input <- getLine
          let action = interp input
          either
            (\err -> putStrLn err >> loop gs)
            ( \action' -> do
                let action'' = validateAction action' gs
                gs' <- execAction action'' gs
                loop gs'
            )
            action

