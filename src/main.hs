module Main where

import Logic.Types
import Logic.Simulation
import Logic.StateManip

import Control.Monad.State
import Control.Concurrent

exampleGameState = addUnit 1 (Position 1 1) $ initialGameState

main = flip execStateT exampleGameState $ replicateM_ 100 $ do
    modify $ simulationStep []
    get >>= liftIO . print
    --liftIO $ threadDelay 1000
    return ()
