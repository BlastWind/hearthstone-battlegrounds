-- Combat phase simulation 
module Combat where

import Model

type Victor = Player
type Damage = Int

-- `fight` simulates the combat and logs every move and intermediate combat state.
fight :: Player -> Player -> GameState -> ([CombatMove], [(Board, Board)], Victor, Damage)
fight p1 p2 gs = ([], [], p1, 5)
