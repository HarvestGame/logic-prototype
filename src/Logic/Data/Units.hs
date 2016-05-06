module Logic.Data.Units where

import Logic.Types
import qualified Data.Map as Map
import Control.Lens

unitsBase :: Map.Map UnitType UnitData
unitsBase = Map.fromList [
    (UnitType 1, marine)
    ]

marine = UnitData {
    _maxHp = 50,
    _attackValue = 8,
    _attackSpeed = 0.5,
    _movementSpeed = 0.5
    }