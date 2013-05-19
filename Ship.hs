module Ship (Ship, initShip) where

import Control.Lens

import Agent
import Animation
import Assets
import Data.Map

data Ship = Ship {
  _shipPosition :: (Float,Float),
  _shipAngle :: Float,
  _shipSpeed :: Float,
  _shipAnimation :: Animation
  } deriving (Eq, Ord, Show)
makeLenses ''Ship

instance Agent Ship where
  position = shipPosition
  angle = shipAngle
  speed = shipSpeed
  animation = shipAnimation
  
initShip :: Assets -> (Float, Float) -> Ship
initShip assets position =
  Ship position (pi/2) 0 ((assets ^. shipAnimations) ! "neutral")
