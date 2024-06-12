-- Logic of the game
module Logic where
import Data.UUID (UUID)
import Model (PlayerState(..), Event(..))
import qualified Data.Map as Map
import Data.Functor
import Control.Arrow ((>>>))

newtype Error = Error String

buy  :: UUID -> PlayerState -> PlayerState
buy = error "buy not implemented"

sell :: UUID -> PlayerState -> PlayerState
sell = error "sell not implemented"

roll :: PlayerState -> PlayerState
roll = error "roll not implemented"

canTierUp :: PlayerState -> Bool
canTierUp ps = curGold ps >= tierUpCost ps && True

tierUpBase :: PlayerState -> PlayerState
tierUpBase ps = ps { curGold = curGold ps - tierUpCost ps, tavernTier = tavernTier ps + 1}

tierUp :: PlayerState -> PlayerState
tierUp ps = foldl (>>>) tierUpBase additionalEvents ps
  where
    additionalEvents = concat $ Map.lookup TierUp (eventHandler ps)