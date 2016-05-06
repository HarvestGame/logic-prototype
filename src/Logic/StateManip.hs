module Logic.StateManip where

import Logic.Types
import qualified Data.Map as Map
import Control.Lens
import Control.Monad.State

unit :: UnitType -> Position -> Unit
unit t p = Unit {
    _UnitState = Moving (PositionTarget $ Position 10 10),
    _attackCooldown = 0,
    _movementCooldown = 0,
    _UnitType = t,

    _position = p,
    _direction = DirN
}

addUnit :: UnitType -> Position -> GameState -> GameState
addUnit t p = execState $ do 
    uid <- do
        UnitId x <- use lastUnitId
        let newId = UnitId x+1
        lastUnitId .= newId
        return newId

    units %= (Map.insert uid (unit t p))

initialGameState = GameState {
    _lastUnitId = UnitId 0,
    _tick = 0,
    _players = Map.fromList [],
    _units = Map.fromList []
    }
