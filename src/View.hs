module View (module View) where

import Control.Concurrent (threadDelay)
import Control.Lens
import Control.Monad (forM_)
import Data.List (intercalate)
import Data.Map (foldrWithKey, keys, (!))
import Debug.Trace (trace)
import Model

rowWidth :: Int
rowWidth = 142

maxCardNameDisplayLength :: Int
maxCardNameDisplayLength = 15

maxRowContentWidth :: Int
maxRowContentWidth = length $ intercalate " | " $ replicate 7 "Rockpool Hunter" -- 123

renderCard :: CardInstance -> String
renderCard ci = abbrev maxCardNameDisplayLength (show (ci ^. card . cardName)) ++ "(" ++ show (ci ^. card . attack) ++ "/" ++ show (ci ^. card . health) ++ ")"

hBorder :: [Char]
hBorder = "+" ++ replicate (rowWidth - 2) '-' ++ "+"

endScreenMsg :: GameState -> PlayerId -> String
endScreenMsg gs pId = if (gs ^. playerMap) ! pId ^. alive then "Victory! Ending now." else "You loss. Ending now."

-- fmtRecruit creates the following string. In the example below, the names and entries are maxed out.
-- I.e., 15 characters is the longest permitting name (Rockpool Hunter and playeracgodman1 have 15 chars). Shop and board have 7 entries max, hand has 10 max.
--
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  |                                                         Recruit                                                                           |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  | Shop:     Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter     |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  | Board:    Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter     |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  | Hand:     Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter | Rockpool Hunter     |
--              Rockpool Hunter | Rockpool Hunter | Rockpool Hunter                                                                             |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  | Tavern:   Upgrade Cost: 6 | Freeze: No | Reroll Cost: 1                                                                                   |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  | Player:   Health: 31 | Armor: 0 | Gold: 7/10                                                                                              |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
--  | Opps HP:  playeracgodman1: 35 + 5 | playeracgodman2: 26 + 3 | playeracgodman3: HP 27 + 0 | playeracgodman4: HP 27 + 3                     |
--  |           playeracgodman5: 35 + 5 | playeracgodman6: 26 + 3 | playeracgodman7: HP 27 + 0                                                  |
--  +-------------------------------------------------------------------------------------------------------------------------------------------+
fmtRecruit :: GameState -> PlayerId -> String
fmtRecruit gs pId =
  intercalate "\n" $
    filter
      (not . null)
      [ hBorder,
        "|" ++ alignMid (rowWidth - 2) "Recruit" ++ "|",
        hBorder,
        "| Shop:     " ++ alignMid maxRowContentWidth (intercalate " | " shopCardNames) ++ "      |",
        hBorder,
        "| Board:    " ++ alignMid maxRowContentWidth (intercalate " | " boardCardNames) ++ "      |",
        hBorder,
        "| Hand:     " ++ alignMid maxRowContentWidth (intercalate " | " $ take 7 handCardNames) ++ "      |",
        if not (null (drop 7 handCardNames))
          then "|           " ++ alignMid maxRowContentWidth (intercalate " | " (drop 7 handCardNames)) ++ "      |"
          else "",
        hBorder,
        "| Tavern:   " ++ alignMid maxRowContentWidth (intercalate " | " [tierText, tierUpCostText, freezeText, rerollCostText]) ++ "      |",
        hBorder,
        "| Player:   " ++ alignMid maxRowContentWidth (intercalate " | " [healthText, armorText, goldText]) ++ "      |",
        hBorder,
        "| Opps HP:  " ++ alignMid maxRowContentWidth oppInfoText ++ "      |",
        hBorder
      ]
  where
    ps = (gs ^. playerMap) ! pId
    shopCardNames = [(abbrev maxCardNameDisplayLength . show) (cardInstance ^. card . cardName) | cardInstance <- ps ^. shop]
    boardCardNames = [(abbrev maxCardNameDisplayLength . show) (cardInstance ^. card . cardName) | cardInstance <- ps ^. board]
    handCardNames = [(abbrev maxCardNameDisplayLength . show) (cardInstance ^. card . cardName) | cardInstance <- ps ^. hand]
    freezeText = if ps ^. frozen then "Freeze: Yes" else "Freeze: No"
    rerollCostText = "Reroll Cost: " ++ show (ps ^. rerollCost)
    tierText = "Tier: " ++ show (ps ^. tier)
    tierUpCostText = "Upgrade Cost: " ++ if ps ^. tier < 6 then show (ps ^. tierUpCost) else "-"
    healthText = "Health: " ++ show (ps ^. hp)
    armorText = "Armor: " ++ show (ps ^. armor)
    goldText = "Gold: " ++ show (ps ^. curGold) ++ "/" ++ show (ps ^. maxGold)
    opps = filter (/= pId) (keys (gs ^. playerMap))
    oppInfoText =
      intercalate
        " | "
        [ show opp
            ++ ": "
            ++ show ((gs ^. playerMap) ! opp ^. hp)
            ++ " + "
            ++ show ((gs ^. playerMap) ! opp ^. armor)
          | opp <- opps
        ]

abbrev :: Int -> String -> String
abbrev maxLen s =
  if length s < maxLen
    then s
    else take (maxLen - 2) s ++ ".."

alignMid :: Int -> String -> String
alignMid space s = leftPad ++ s ++ rightPad
  where
    spacesLeft = space - length s
    carry = spacesLeft `mod` 2
    leftPadCnt = spacesLeft `div` 2 + carry
    rightPadCnt = spacesLeft `div` 2
    leftPad = replicate leftPadCnt ' '
    rightPad = replicate rightPadCnt ' '

-- Replay the combat by rendering each "slice" of the combat state x seconds apart.
type Seconds = Double

replayCombat :: Seconds -> CombatSimulation -> IO ()
replayCombat secs (CombatSimulation _ bs result) = do
  -- [CombatMove] is ignored for now. But, they are required to flavor the move UI (i.e., animate an attack require knowing who attacked who)
  forM_ (map renderCombatBoardState bs) $ \s -> do
    putStrLn s
    threadDelay $ round $ secs * 1000
  case result of
    Tie -> putStrLn "You tied the round. Bob: Welcome back! How's it going out there?"
    Loss loser dmg -> if loser == One then putStrLn $ "You lost the round and took " ++ show dmg ++ " dmg. Bob: You're good at this!" else putStrLn $ "You won the round and dealt " ++ show dmg ++ " dmg! Bob: I think you can win this thing!"

renderCombatBoardState :: (Board, Board) -> String
renderCombatBoardState (board1, board2) =
  intercalate "\n" $
    [ hBorder,
      "|" ++ alignMid (rowWidth - 2) "Combat Simulation" ++ "|",
      hBorder,
      "| Player 2: " ++ alignMid maxRowContentWidth (intercalate " | " (map renderCard board2)) ++ "      |",
      hBorder,
      "| Player 1: " ++ alignMid maxRowContentWidth (intercalate " | " (map renderCard board1)) ++ "      |",
      hBorder
    ]

helpMenu :: String
helpMenu =
  intercalate
    "\n"
    [ "+-----------------------------------------------------+",
      "|                      HELP MENU                      |",
      "+-----------------------------------------------------+",
      "| Command              | Description                  |",
      "+-----------------------------------------------------+",
      "| buy <n> or b <n>     | Buy card at shop pos <n>     |",
      "| sell <n> or s <n>    | Sell minion at board pos <n> |",
      "| play <n> or p <n>    | Play minion at hand pos <n>  |",
      "| roll or r            | Refresh your tavern          |",
      "| tier or t            | Tier up your tavern          |",
      "| freeze or f          | Freeze your tavern           |",
      "| endturn or e         | End your turn                |",
      "| help or h            | Display this menu            |",
      "| concede              | Concede!                     |",
      "+-----------------------------------------------------+",
      "| Note: <n> is a number that starts at 0.             |",
      "| There's also no need to have space before <n> arg   |",
      "+-----------------------------------------------------+"
    ]
