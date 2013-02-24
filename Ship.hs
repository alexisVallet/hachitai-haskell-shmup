module Ship 
       (
         Ship,
         newShip,
         loadShipAssets
       ) where

import Graphics.UI.SDL
import Control.Monad.Reader.Class
import Control.Lens
import Data.NumInstances ()
import Data.VectorSpace
import Control.Arrow hiding (left, right)
import Control.Monad.IO.Class
import Data.Maybe

import KinematicAgent
import AnimatedAgent
import Environment
import Collision
import Animation
import Assets
import Config

data Ship = Ship {
  _shipPosition :: (Float,Float),
  _shipSpeed :: Float,
  _shipAngle :: Float,
  _shipCurrentAnimation :: AnimationPoint
  } deriving (Eq, Ord, Show)
makeLenses ''Ship

instance AnimatedAgent Ship where
  currentAnimation = shipCurrentAnimation

instance KinematicAgent Ship where
  position = shipPosition
  speed = shipSpeed
  angle = shipAngle

instance HasAABB Ship where
  getAABB ship = AABB (ship^.position) (2,2)

instance AgentClass Ship where
  live dt = do
    processInputs
    updateAnimation
    updatePosition dt
    let 
      wrap maxValue x = max 0 $ min maxValue x
      wrapInScreen (x,y) =
        (wrap (fromIntegral screenWidth) x,
         wrap (fromIntegral screenHeight) y)
    agent.position %= wrapInScreen
  agentType _ = ShipType
  render ship =
    let 
      surface = currentFrame $ ship^.currentAnimation
      (w,h) = (surfaceGetWidth surface, surfaceGetHeight surface)
      (x,y) = (round *** round) $ ship^.position
      rect = Rect (x-(w`div`2)) (y-(h`div`2)) w h in
    (surface,rect,0)

loadShipAssets :: MainMonad ()
loadShipAssets = do
  walking <- fmap fromJust
             $ liftIO 
             $ loadAnimation "../../Test/animation" (2000`div`framerate)
  let loopedWalking = fromJust $ loopFrom 0 walking
  addAnimation "walkingLink" loopedWalking

newShip :: (Float,Float) -> GameMonad Agent
newShip initPosition = do
  mAnimation <- getAnimation "walkingLink"
  case mAnimation of
    Nothing -> error $ "Cannot get animation walkingLink"
    Just animation -> do
      animationPoint <- startAnimation animation
      newAgent $ Ship {
        _shipPosition = initPosition,
        _shipSpeed = 0,
        _shipAngle = pi/2,
        _shipCurrentAnimation = animationPoint
        }

maxSpeed :: Float
maxSpeed = 100

processInputs :: AgentMonad Ship ()
processInputs = do
  inputs <- query getInputs
  let (moving, angle') =
        case (inputs^.up,inputs^.down,inputs^.left,inputs^.right) of
          (True,False,False,False) -> (True,pi/2)
          (False,True,False,False) -> (True,-pi/2)
          (False,False,True,False) -> (True,pi)
          (False,False,False,True) -> (True,0)
          (True,False,True,False) -> (True,3*pi/4)
          (True,False,False,True) -> (True,pi/4)
          (False,True,True,False) -> (True,-3*pi/4)
          (False,True,False,True) -> (True,-pi/4)
          _ -> (False, pi/2)
  agent.speed .= if moving then maxSpeed else 0
  agent.angle .= angle'
