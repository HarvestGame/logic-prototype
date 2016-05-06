module Logic.Simulation where

import Logic.Types
import Logic.Data.Units
import Control.Lens
import Control.Monad.State
import Data.Maybe (fromJust)
import qualified Data.Map as Map (lookup, fromList)

tickTime = 1.0/60.0 :: Float

simulationStep :: PlayerCommandSet -> GameState -> GameState
simulationStep cmds g = flip execState g $ do
    tick += 1
    -- first we want to advance all units that are already in motion
    units . traversed . movementCooldown %= cooldown
    units . traversed . filtered isMoving  . attackCooldown %= cooldown

    -- move the ones that are moving
    units . traversed %= move

    -- then see if any of those is ready to make an action
    --actions <- units . traversed %= makeAction

    return ()

isMoving :: Unit -> Bool
isMoving u = case u ^. unitState of
    Moving {} -> True
    _ -> False

-- | The sub-tick values can only be used in the next following tick
cooldown a
    | a == 0 = 0
    | a < 0 = 0
    | otherwise = a - tickTime

applyMovement :: Direction -> Position -> Position
applyMovement d (Position x y) = Position (x+dX) (y+dY)
    where 
        Position dX dY = toMovement d

toMovement :: Direction -> Position
toMovement DirN = Position 0 (-1)
toMovement DirNE = Position 1 (-1)
toMovement DirNW = Position (-1) (-1)
toMovement DirW = Position (-1) 0
toMovement DirE = Position 1 0
toMovement DirS = Position 0 1
toMovement DirSE = Position 1 1
toMovement DirSW = Position (-1) 1

move :: Unit -> Unit
move = execState $ do
    mcd <- use movementCooldown
    tp <- use unitType

    let unitData = fromJust $ Map.lookup tp unitsBase
    let speed = unitData ^. movementSpeed

    if mcd <= 0
         then do
            (position %=) . applyMovement =<< use direction
            movementCooldown .= speed
            (direction .=) =<< calculateDirection

         else return ()

-- | This function should probably be replaced by actual pathfinding
calculateDirection :: State Unit Direction
calculateDirection = do
    (Position x y) <- use position
    (Position tx ty) <- findTargetPosition

    let dx = tx - x
    let dy = ty - y

    return $
        if dx == 0 then
            if dy == 0 then DirN -- on target, essentially impossible
                else if dy > 0 then DirS
                               else DirN
        else if dx > 0 then
            if dy == 0 then DirE
                else if dy > 0 then DirSE
                               else DirNE
        else if dy == 0 then DirW
                else if dy > 0 then DirSW
                               else DirNW

findTargetPosition :: State Unit Position
findTargetPosition = do
    (Moving t) <- use unitState
    case t of
        (PositionTarget p) -> return p
        (UnitTarget uid) -> error "not implemented yet, also change signature"