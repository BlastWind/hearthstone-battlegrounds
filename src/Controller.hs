-- Controller: Handles input, game loop

module Controller (module Controller) where

import Card (bigDumbo)
import Control.Monad.Random (MonadRandom (getRandom))
import Logic (execCommand, isGameOver, validateCommand, enter)
import Model
import Text.Parsec hiding (Error)
import Text.Parsec.String (Parser)
import Text.Read (readMaybe)
import View (render)
import Control.Monad.IO.Class (liftIO)

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
  cmd <- choice $ map (try . string) ["buy", "b", "sell", "s", "help", "h", "endturn", "e", "play", "p"]
  spaces
  action <- actionArgumentParser cmd
  eof
  return action

-- Dispatch argument parser depending on command
actionArgumentParser :: String -> Parser Command
actionArgumentParser cmd
  | cmd `elem` ["buy", "b"] = buyArgParser
  | cmd `elem` ["sell", "s"] = sellArgParser
  | cmd `elem` ["play", "p"] = playArgParser
  | cmd `elem` ["help", "h"] = return Help
  | cmd `elem` ["endturn", "e"] = return EndTurn
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
  return $ GameState {playerState = defPlayerState, aiState=tutorialAIGameState, turn = 0}

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
      frozen = False,
      hp = 20,
      armor = 0,
      alive = True,
      phase = HeroSelect,
      combatSequence = ([], 0)
    }

runGame :: IO ()
runGame = do
  gs <- initGameState
  gs' <- enter Recruit gs
  _ <- loop gs'
  putStrLn "Game Finished."
  where
    -- Repeat Recruit and Combat until game over
    loop gs =
      if isGameOver gs
        then do
          return gs
        else do
          putStrLn $ render gs Player
          input <- getLine
          case interp input >>= (\c -> validateCommand c gs Player) of
            Left err -> putStrLn err >> loop gs
            Right action' -> do
              gs' <- liftIO $ execCommand action' gs Player
              loop gs' 

