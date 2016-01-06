module Logic.Validation where

import Logic.Types
import Control.Lens
import Data.Maybe
import qualified Data.Map as Map

validateCommand :: GameState -> Command -> Bool
validateCommand gs (MoveCommand us target) = 
    all id [allUnitsExist, targetOk target]
    where
        allUnitsExist = all id . map doesExist $ us
        doesExist u = isJust $ Map.lookup u (gs ^. units)

        targetOk (PositionTarget p) = True
        targetOk (UnitTarget u) = doesExist u
