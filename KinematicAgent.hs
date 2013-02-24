module KinematicAgent where

import Control.Lens
import Data.NumInstances ()
import Data.VectorSpace

import Environment
import Collision

class (AgentClass a) => KinematicAgent a where
  position :: Simple Lens a (Float,Float)
  angle :: Simple Lens a Float
  speed :: Simple Lens a Float

updatePosition :: (KinematicAgent a) => Float -> AgentMonad a ()
updatePosition dt = do
  agent' <- use agent
  agent.position += 
    (agent'^.speed * dt) *^ (cos $ agent'^.angle,-sin $ agent'^.angle)
