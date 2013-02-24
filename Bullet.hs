module Bullet 
       (
         Bullet,
         newBullet,
         loadBulletAssets
       ) where

import Control.Lens
import Data.NumInstances ()
import Data.VectorSpace

import Environment
import Collision
import Animation
import Assets
import Config

data Bullet = Bullet {
  _position :: (Float,Float),
  _angle :: Float,
  _speed :: Float,
  _currentAnimation :: AnimationPoint
  }
makeLenses ''Bullet

instance HasAABB Bullet where
  getAABB bullet = AABB (bullet^.position) (2,2)

instance AgentClass Bullet where
  live dt ident bullet = do
    animationPoint <- nextFrame $ ship'^.currentAnimation
    let
      bullet' = bullet {
        _position = 
           bullet^.position 
           + (bullet^.speed * dt) 
           *^ (cos $ bullet^.angle,sin $ bullet^.angle),
        _currentAnimation <- animationPoint
        }
    agents %= move ident bullet bullet'
  agentType _ = BulletType
  render bullet =
    let
      surface = currentFrame $ bullet^.currentAnimation
      