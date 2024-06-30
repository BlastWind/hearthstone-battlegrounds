module View.Terminal (render, helpMenu) where

import Data.List (intercalate)
import Data.Map (toList)
import Model (Card (_cardName), CardInstance (..), OppInfo (oppArmor, oppHP), Phase (..), PlayerState (..))

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

maxPlayerNameDisplayLength :: Int
maxPlayerNameDisplayLength = 15

maxRowContentWidth :: Int
maxRowContentWidth = length $ intercalate " | " $ replicate 7 "Rockpool Hunter" -- 123

render :: PlayerState -> String
render ps =
  case phase ps of
    Recruit -> renderRecruit ps
    Blank -> "blank todo"
    HeroSelect -> "heroselect todo"
    Combat -> "combat todo"

hBorder :: [Char]
hBorder = "+" ++ replicate (rowWidth - 2) '-' ++ "+"

renderRecruit :: PlayerState -> String
renderRecruit ps =
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
        "| Tavern:   " ++ alignMid maxRowContentWidth (intercalate " | " [tierUpCostText, freezeText, rerollCostText]) ++ "      |",
        hBorder,
        "| Player:   " ++ alignMid maxRowContentWidth (intercalate " | " [healthText, armorText, goldText]) ++ "      |",
        hBorder,
        "| Opps HP:  " ++ alignMid maxRowContentWidth (intercalate " | " $ take 4 oppInfoTextList) ++ "      |",
        if not (null (drop 4 oppInfoTextList))
          then "|           " ++ alignMid maxRowContentWidth (intercalate " | " $ drop 4 oppInfoTextList) ++ "      |"
          else "",
        hBorder
      ]
  where
    shopCardNames = [(abbrev maxCardNameDisplayLength . show . _cardName . _card) cardInstance | cardInstance <- shop ps]
    boardCardNames = [(abbrev maxCardNameDisplayLength . show . _cardName . _card) cardInstance | cardInstance <- board ps]
    handCardNames = [(abbrev maxCardNameDisplayLength . show . _cardName . _card) cardInstance | cardInstance <- hand ps]
    freezeText = if frozen ps then "Freeze: Yes" else "Freeze: No"
    rerollCostText = "Reroll Cost: " ++ show (rerollCost ps)
    tierUpCostText = "Upgrade Cost: " ++ show (tierUpCost ps)
    healthText = "Health: " ++ show (hp ps)
    armorText = "Armor: " ++ show (armor ps)
    goldText = "Gold: " ++ show (curGold ps) ++ "/" ++ show (maxGold ps)
    oppInfoTextList = [abbrev maxPlayerNameDisplayLength userName ++ ": " ++ show (oppHP info) ++ " + " ++ show (oppArmor info) | (userName, info) <- toList $ opponentInformation ps]

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
    [ "+--------------------------------------------+",
      "|                  HELP MENU                 |",
      "+--------------------------------------------+",
      "| Command         | Description              |",
      "+--------------------------------------------+",
      "| buy <n>         | Buy card at index <n>    |",
      "| b <n>           | Shortcut for buy <n>     |",
      "| sell <n>        | Sell minion at index <n> |",
      "| s <n>           | same as sell <n>         |",
      "| help            | Display this menu        |",
      "| h               | Shortcut for help        |",
      "| endturn         | End your turn            |",
      "| e               | Shortcut for endturn     |",
      "+--------------------------------------------+",
      "| Note: number argument <n> starts at 1      |",
      "+--------------------------------------------+"
    ]
