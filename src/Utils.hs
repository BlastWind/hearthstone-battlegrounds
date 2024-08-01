{-# LANGUAGE OverloadedRecordDot #-}

module Utils (module Utils) where
import Model

updatePlayer :: Player -> PlayerState -> GameState -> GameState
updatePlayer Player ps gs = gs { playerState = ps }
updatePlayer AI     ps gs = gs { aiState     = ps}

selectPlayer :: Player -> GameState -> PlayerState
selectPlayer Player gs = gs.playerState
selectPlayer AI     gs = gs.aiState