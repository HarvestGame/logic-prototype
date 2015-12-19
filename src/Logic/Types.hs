{-# LANGUAGE TemplateHaskell #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE FlexibleContexts #-}

module Logic.Types where

import Control.Lens
import qualified Data.Map as Map
import Control.Monad.State.Class

type Id = Int
newtype PlayerId = PlayerId Id
newtype UnitId = UnitId Id

data UnitState = Idle | Moving | Harvesting | Fighting

data Unit = Unit {
    _UnitState :: UnitState
    }

data Building = Building {
    _buildingHp :: Int,
    _buildingMaxHp :: Int
    }

-- | This is a complete snapshot of a game.
data GameState = GameState {
    _units :: Map.Map UnitId Unit,
    _tick :: Int -- | this is the tick number of the current game
    }

-- | Universal type for stateful contexts.
type GameM a = forall m. MonadState GameState m => m a

-- | Represents continuous position on the game map.
data Position = Position Int Int

-- | Represents a position used on the grid by building units.
data GridPosition = GridPosition Int Int


-- | Represents a possible
data Target = PositionTarget Position | UnitTarget UnitId

-- | One atomic input coming from a player.
data Command = MoveCommand UnitId Target | AttackCommand 

-- | Represents input coming from a player.
-- A player can submit more than one command for each tick.
data PlayerCommand = PlayerCommand {
    _playerId :: PlayerId,
    _command :: Command
    }
type PlayerCommandSet = [PlayerCommand]
