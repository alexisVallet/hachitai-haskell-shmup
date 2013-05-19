module Environment where

import Control.Lens

import Config
import Assets
import Ship
import Bullet

defaultShipPosition :: (Float,Float)
defaultShipPosition = (realToFrac screenHeight * 3 / 4, realToFrac screenWidth / 2)

data Environment = Environment {
  _ship :: Maybe Ship,
  _bullets :: [Bullet]
  } deriving (Eq, Ord, Show)
makeLenses ''Environment

initEnvironment :: Assets -> Environment
initEnvironment assets =
  let ship = initShip assets defaultShipPosition
      bullets = [] in
  Environment (Just ship) bullets