module AnimatedAgent where

import Control.Lens

import Environment
import Animation

class (AgentClass a) => AnimatedAgent a where
  currentAnimation :: Simple Lens a AnimationPoint

updateAnimation :: (AnimatedAgent a) => AgentMonad a ()
updateAnimation = do
  animationPoint <- use $ agent.currentAnimation
  updated <- nextFrame animationPoint
  agent.currentAnimation .= updated
