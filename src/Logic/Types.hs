{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Logic.Types where

import Control.Lens
import qualified Data.Map as Map
import Control.Monad.State.Class

-- | Id is something that tags an instance of something
type Id = Int

newtype PlayerId = PlayerId Id deriving (Show, Eq, Ord, Num)
newtype UnitId = UnitId Id deriving (Show, Eq, Ord, Num)
newtype UnitType = UnitType Id deriving (Show, Eq, Ord, Num)

-- | Kind tags a static type of something
type Kind = Int
newtype BuildingKind = BuildingKind Kind

data UnitData = UnitData {
    _maxHp :: Int,
    _attackValue :: Int,
    _attackSpeed :: Float,
    _movementSpeed :: Float
}
makeLenses ''UnitData

-- | Represents continuous position on the game map.
data Position = Position Int Int deriving (Show, Eq)

-- | Represents a direction on the grid
data Direction = 
    DirNW | DirN | DirNE |
    DirW  |        DirE  |
    DirSW | DirS | DirSE
    deriving (Show, Eq)

-- | Represents a possible target for a command
data Target = PositionTarget Position | UnitTarget UnitId deriving (Show, Eq)

data UnitState = Idle | Moving Target | Harvesting Target | Attacking Target deriving (Show)

data Unit = Unit {
    _UnitState :: UnitState,
    _attackCooldown :: Float,
    _movementCooldown :: Float,
    _UnitType :: UnitType,

    _position :: Position,
    _direction :: Direction
    } deriving (Show)
makeLenses ''Unit

data PlayerState = PlayerState {
    _gold :: Int,
    _wood :: Int
    } deriving (Show)

-- | This is a complete snapshot of a game.
data GameState = GameState {
    _units :: Map.Map UnitId Unit,
    _lastUnitId :: UnitId,
    _tick :: Int, -- | this is the tick number of the current game
    _players :: Map.Map PlayerId PlayerState
    } deriving (Show)
makeLenses ''GameState

-- | Universal type for stateful contexts.
type GameM a = forall m. MonadState GameState m => m a

-- | Represents a position used on the grid by building units.
data GridPosition = GridPosition Int Int

-- | One atomic input coming from a player. Note that one
-- command can be given to multiple units, except of BuildCommand.
data Command =
    MoveCommand [UnitId] Target |
    AttackCommand [UnitId] Target |
    BuildCommand UnitId BuildingKind GridPosition |
    GatherCommand [UnitId] Target

-- | Represents input coming from a player.
-- A player can submit more than one command for each tick.
data PlayerCommand = PlayerCommand {
    _playerId :: PlayerId,
    _command :: Command
    }
type PlayerCommandSet = [PlayerCommand]
