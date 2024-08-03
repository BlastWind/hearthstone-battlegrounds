{-# LANGUAGE ConstraintKinds        #-}
{-# LANGUAGE DataKinds              #-}
{-# LANGUAGE DuplicateRecordFields  #-}
{-# LANGUAGE FlexibleInstances      #-}
{-# LANGUAGE GADTs                  #-}
{-# LANGUAGE MultiParamTypeClasses  #-}
{-# LANGUAGE OverloadedRecordUpdate #-}
{-# LANGUAGE RebindableSyntax       #-}
{-# LANGUAGE TypeFamilies           #-}
{-# LANGUAGE OverloadedRecordDot    #-}
{-# OPTIONS_GHC -fplugin=Data.Record.Plugin #-}

module Utils (module Utils) where
import Model
import Data.Record.Overloading


updatePlayer :: Player -> PlayerState -> GameState -> GameState
updatePlayer Player ps gs = gs { playerState = ps }
updatePlayer AI     ps gs = gs { aiState     = ps}

selectPlayer :: Player -> GameState -> PlayerState
selectPlayer Player gs = gs.playerState
selectPlayer AI     gs = gs.aiState