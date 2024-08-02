{-# LANGUAGE OverloadedRecordDot #-}

module View (render, helpMenu) where

import Data.List (intercalate)
import Model
import Utils (selectPlayer)
import Control.Monad (forM_)
import Control.Concurrent (threadDelay)

-- Render creates the following example. In the example, the names and entries are maxed out.
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

rowWidth :: Int
rowWidth = 142

maxCardNameDisplayLength :: Int
maxCardNameDisplayLength = 15

maxRowContentWidth :: Int
maxRowContentWidth = length $ intercalate " | " $ replicate 7 "Rockpool Hunter" -- 123

render :: GameState -> Player -> IO ()
render gs p =
  case (selectPlayer p gs).phase of
    Recruit -> putStrLn $ renderRecruit gs p
    HeroSelect -> putStrLn "heroselect todo"
    Combat -> forM_ (replayCombat gs.playerState.combatSimulation) $ \s -> do
      putStrLn s
      threadDelay $ 500 * 1000 -- 500 milliseconds
    EndScreen -> if (selectPlayer p gs).alive then putStrLn "Victory! Ending now." else putStrLn "You loss. Ending now."

-- [String] contains each slice of the readily renderable combat!
-- [CombatMove] is ignored for now. But, they are required to flavor the UI
replayCombat :: CombatSimulation -> [String]
replayCombat (CombatSimulation _ bs) = map renderBoardState bs

renderBoardState :: ([CardInstance], [CardInstance]) -> String
renderBoardState (board1, board2) =
  intercalate "\n" $
    [ hBorder,
      "|" ++ alignMid (rowWidth - 2) "Combat Simulation" ++ "|",
      hBorder,
      "| Player 1: " ++ alignMid maxRowContentWidth (intercalate " | " (map renderCard board1)) ++ "      |",
      hBorder,
      "| Player 2: " ++ alignMid maxRowContentWidth (intercalate " | " (map renderCard board2)) ++ "      |",
      hBorder
    ]

renderCard :: CardInstance -> String
renderCard ci = abbrev maxCardNameDisplayLength (show ci.card.cardName) ++ "(" ++ show ci.card.attack ++ "/" ++ show ci.card.health ++ ")"

hBorder :: [Char]
hBorder = "+" ++ replicate (rowWidth - 2) '-' ++ "+"

renderRecruit :: GameState -> Player -> String
renderRecruit gs p =
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
    ps = selectPlayer p gs
    shopCardNames = [(abbrev maxCardNameDisplayLength . show) cardInstance.card.cardName | cardInstance <- shop ps]
    boardCardNames = [(abbrev maxCardNameDisplayLength . show) cardInstance.card.cardName | cardInstance <- board ps]
    handCardNames = [(abbrev maxCardNameDisplayLength . show) cardInstance.card.cardName | cardInstance <- hand ps]
    freezeText = if frozen ps then "Freeze: Yes" else "Freeze: No"
    rerollCostText = "Reroll Cost: " ++ show (rerollCost ps)
    tierText = "Tier: " ++ show ps.tier
    tierUpCostText = "Upgrade Cost: " ++ if ps.tier < 6 then show (tierUpCost ps) else "-"
    healthText = "Health: " ++ show (hp ps)
    armorText = "Armor: " ++ show (armor ps)
    goldText = "Gold: " ++ show (curGold ps) ++ "/" ++ show (maxGold ps)
    oppInfoText = "Tutorial AI" ++ ": " ++ show gs.aiState.hp ++ " + " ++ show gs.aiState.armor

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
