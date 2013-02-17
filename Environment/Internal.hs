module Environment.Internal where

import Control.Lens
import Control.Lens.TH
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.State.Class
import Control.Monad.Reader.Class

-- | AABB with center and half dimension
data AABB = AABB (Float,Float) (Float,Float)
            deriving (Eq, Ord, Show)

data Quadrant = NW | NE | SW | SE

-- | QuadTree data structure containing AABBs
data QuadTree a = Leaf
                | Node AABB [a] (QuadTree a) (QuadTree a)
                                (QuadTree a) (QuadTree a)
                  deriving (Eq, Ord, Show)

class HasAABB a where
  getAABB :: a -> AABB

instance HasAABB AABB where
  getAABB = id

class HasID a where
  getID :: a -> Int

class (Show a, HasAABB a) => AgentClass a where
  live :: Float -> Int -> a -> GameMonad ()
  agentType :: a -> AgentType
  render :: a -> (Surface,Rect,Int)

data AgentType = ShipType
               | EnemyType
               | EnemyBulletType
               | PlayerBulletType
               | OtherType

data Agent where
  Agent :: AgentClass a => Int -> a -> Agent

instance HasAABB Agent where
  getAABB (Agent _ a) = getAABB a

instance HasID Agent where
  getID (Agent ident _) = ident

instance Show Agent where
  show (Agent ident agent) = "Agent " ++ show ident ++ ": " ++ show agent

data Environment = Environment {
  _agents :: QuadTree Agent,
  _currentId :: Int
  }

data Inputs = Inputs {
  _up :: Bool,
  _down :: Bool,
  _left :: Bool,
  _right :: Bool,
  _shot :: Bool,
  _laser :: Bool,
  _pause :: Bool
  }

-- | Base monad, can modify the environment
type EnvMonadT = StateT Environment
-- | The game monad can modify the environment and read user inputs, and
-- cannot perform any IO.
type GameMonad = ReaderT Inputs (EnvMonadT Identity)
-- | The main monad is "more privileged" and can also modify the
-- inputs from IO information.
type MainMonad = StateT Inputs (EnvMonadT IO)

makeLenses ''Environment
makeLenses ''Inputs