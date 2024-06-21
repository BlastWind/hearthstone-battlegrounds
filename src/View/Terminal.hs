{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Use list literal" #-}
module View.Terminal where
import Model (PlayerState)
import Data.List (intercalate)

render :: PlayerState -> String
render ps = "Player State Representation"

helpMenu :: String
helpMenu = intercalate "\n" $
  "+--------------------------------------+":
  "|              HELP MENU               |":
  "+--------------------------------------+":
  "| Command         | Description        |":
  "+--------------------------------------+":
  "| buy <n>         | Buy item <n>       |":
  "| b <n>           | Shortcut for buy   |":
  "| sell <n>        | Sell item <n>      |":
  "| s <n>           | Shortcut for sell  |":
  "| help            | Display this menu  |":
  "| h               | Shortcut for help  |":
  "| endturn         | End your turn      |":
  "| e               | Shortcut for endturn|":
  "| someaction <a>  | Perform some action|":
  "| m <a1> <a2>     | Another action     |":
  "+--------------------------------------+":
  "| Note: <n> and <a> represent arguments|":
  "+--------------------------------------+":
  []