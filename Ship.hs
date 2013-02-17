module Ship 
       (
         Ship,
         newShip
       ) where

import Environment

import Control.Monad.Reader.Class
import Control.Lens
import Data.NumInstances ()
import Data.VectorSpace

data Ship = Ship {
  _position :: (Float,Float),
  _speed :: Float,
  _angle :: Float
  } deriving (Eq, Ord, Show)
makeLenses ''Ship

instance HasAABB Ship where
  getAABB ship = AABB (ship^.position) (2,2)

instance AgentClass Ship where
  live dt ident ship = do
    ship' <- fromInputs ship
    let ship'' = ship' {
          _position = 
             ship'^.position 
             + (ship'^.speed * dt) *^ (cos $ ship'^.angle,sin $ ship'^.angle)
          }
    agents %= move ident ship ship''
  agentType _ = ShipType

newShip :: (Float,Float) -> GameMonad Agent
newShip initPosition = newAgent $ Ship {
  _position = initPosition,
  _speed = 0,
  _angle = pi/2
  }

maxSpeed :: Float
maxSpeed = 3

fromInputs :: Ship -> GameMonad Ship
fromInputs ship = do
  inputs <- ask
  let (moving, angle) =
        case (inputs^.up,inputs^.down,inputs^.left,inputs^.right) of
          (True,False,False,False) -> (True,pi/2)
          (False,True,False,False) -> (True,-pi/2)
          (False,False,True,False) -> (True,pi)
          (False,False,False,True) -> (True,0)
          _ -> (False, pi/2)
  return $ ship {
    _speed = if moving then maxSpeed else 0,
    _angle = angle
    }
