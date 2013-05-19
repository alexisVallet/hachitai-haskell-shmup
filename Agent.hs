module Agent where

import Data.NumInstances
import Data.VectorSpace
import Control.Lens
import Animation

class Agent a where
  position :: SimpleLens a (Float, Float)
  angle :: SimpleLens a Float
  speed :: SimpleLens a Float
  animation :: SimpleLens a Animation

updateAgent :: (Agent a) => a -> a
updateAgent agent =
  let newPosition = agent ^. position + (agent ^. speed *^ (-sin $ agent ^.angle, cos $ agent ^. angle))
      newAnimation = tail $ agent ^. animation in 
  set position newPosition $ set animation newAnimation agent