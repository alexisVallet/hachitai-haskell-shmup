module Bullet (Bullet, newBullet) where

import Graphics.UI.SDL
import Control.Lens
import Data.Map

import Agent
import Animation
import Assets

data Bullet = Bullet {
  _bulletPosition :: (Float,Float),
  _bulletAngle :: Float,
  _bulletSpeed :: Float,
  _bulletAnimation :: Animation
  } deriving (Eq, Ord, Show)
makeLenses ''Bullet

instance Agent Bullet where
  position = bulletPosition
  angle = bulletAngle
  speed = bulletSpeed
  animation = bulletAnimation

newBullet :: Assets -> String -> (Float, Float) -> Float -> Float -> Bullet
newBullet assets animationName position angle speed =
  Bullet position angle speed ((assets ^. bulletAnimations) ! animationName)