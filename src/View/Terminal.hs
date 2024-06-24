{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}

{-# HLINT ignore "Use list literal" #-}
module View.Terminal where

import Data.List (intercalate)
import Data.Map (toList)
import Model (Card (_cardName), CardInstance (..), OppInfo (oppArmor, oppHP), PlayerState (..))

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

maxCardNameDisplayLength :: Integer
maxCardNameDisplayLength = 15

maxPlayerNameDisplayLength :: Integer
maxPlayerNameDisplayLength = 15

maxRowContentWidth :: Int
maxRowContentWidth = length $ intercalate " | " $ replicate 7 "Rockpool Hunter"

render :: PlayerState -> String
render = renderRecruit

renderRecruit :: PlayerState -> String
renderRecruit ps =
  intercalate "\n" $
    filter
      (not . null)
      [ "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "|                                                         Recruit                                                                           |",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "| Shop:     " ++ alignMid maxRowContentWidth (intercalate " | " shopCardNames) ++ "     |",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "| Board:    " ++ alignMid maxRowContentWidth (intercalate " | " boardCardNames) ++ "     |",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "| Hand:     " ++ alignMid maxRowContentWidth (intercalate " | " $ take 7 handCardNames) ++ "     |",
        if not (null (drop 7 handCardNames))
          then "|           " ++ alignMid maxRowContentWidth (intercalate " | " (drop 7 handCardNames)) ++ "     |"
          else "",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "| Tavern:   " ++ alignMid maxRowContentWidth (intercalate " | " [tierUpCostText, freezeText, rerollCostText]) ++ "     |",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "| Player:   " ++ alignMid maxRowContentWidth (intercalate " | " [healthText, armorText, goldText]) ++ "     |",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+",
        "| Opps HP:  " ++ alignMid maxRowContentWidth (intercalate " | " $ take 4 oppInfoTextList) ++ "     |",
        if not (null (drop 4 oppInfoTextList))
          then "|           " ++ alignMid maxRowContentWidth (intercalate " | " $ drop 4 oppInfoTextList) ++ "     |"
          else "",
        "+-------------------------------------------------------------------------------------------------------------------------------------------+"
      ]
  where
    shopCardNames = [(abbrev 15 . show . _cardName . _card) cardInstance | cardInstance <- shop ps]
    boardCardNames = [(abbrev 15 . show . _cardName . _card) cardInstance | cardInstance <- board ps]
    handCardNames = [(abbrev 15. show . _cardName . _card) cardInstance | cardInstance <- hand ps]
    freezeText = if frozen ps then "Freeze: Yes" else "Freeze: No"
    rerollCostText = "Reroll Cost: " ++ show (rerollCost ps)
    tierUpCostText = "Upgrade Cost: " ++ show (tierUpCost ps)
    healthText = "Health: " ++ show (hp ps)
    armorText = "Armor: " ++ show (armor ps)
    goldText = "Gold: " ++ show (curGold ps) ++ "/" ++ show (maxGold ps)
    oppInfoTextList = [abbrev 15 (show userName) ++ ": " ++ show (oppHP info) ++ " + " ++ show (oppArmor info) | (userName, info) <- toList $ opponentInformation ps]

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
  intercalate "\n" $
    "+--------------------------------------------+"
      : "|                  HELP MENU                 |"
      : "+--------------------------------------------+"
      : "| Command         | Description              |"
      : "+--------------------------------------------+"
      : "| buy <n>         | Buy card at index <n>    |"
      : "| b <n>           | Shortcut for buy <n>     |"
      : "| sell <n>        | Sell minion at index <n> |"
      : "| s <n>           | same as sell <n>         |"
      : "| help            | Display this menu        |"
      : "| h               | Shortcut for help        |"
      : "| endturn         | End your turn            |"
      : "| e               | Shortcut for endturn     |"
      : "+--------------------------------------------+"
      : "| Note: number argument <n> starts at 1      |"
      : "+--------------------------------------------+"
      : []
