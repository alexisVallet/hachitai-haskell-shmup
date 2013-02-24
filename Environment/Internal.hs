module Environment.Internal 
       (
         HasID(..),
         AgentClass(..),
         AgentType(..),
         Agent(..),
         Environment(..),
         agents,
         currentId,
         assets,
         currentTime,
         Inputs(..),
         up,
         down,
         left,
         right,
         shot,
         laser,
         pause,
         AssetsReader(..),
         InputsReader(..),
         EnvironmentState(..),
         GameState(..),
         inputs,
         GameReader(..),
         GameMonad,
         MainState(..),
         MainMonad,
         AgentState(..),
         agent,
         AgentReader(..),
         AgentMonad
       ) where

import Graphics.UI.SDL
import Control.Lens
import Control.Lens.TH
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Control.Monad.State.Class
import Control.Monad.Reader.Class

import Assets.Internal
import QuadTree
import MonadTime

class HasID a where
  getID :: a -> Int

class (Show a, HasAABB a) => AgentClass a where
  live :: Float -> AgentMonad a ()
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
  _currentId :: Int,
  _currentTime :: Int
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

data GameState = GameState {
  _gameEnvironment :: Environment
  }
data GameReader = GameReader {
  _gameInputs :: Inputs,
  _gameAssets :: Assets
  }
type GameMonad = RWS GameReader () GameState
data MainState = MainState {
  _mainEnvironment :: Environment,
  _inputs :: Inputs,
  _assets :: Assets
  }
type MainMonad = StateT MainState IO
data AgentState a = AgentState {
  _agentEnvironment :: Environment,
  _agent :: a
  }
data AgentReader = AgentReader {
  _agentInputs :: Inputs,
  _agentAssets :: Assets,
  _ident :: Int
  }
type AgentMonad a = RWS AgentReader () (AgentState a)

makeLenses ''Environment
makeLenses ''Inputs
makeLenses ''MainState
makeLenses ''GameState
makeLenses ''GameReader
makeLenses ''AgentState
makeLenses ''AgentReader

class AssetsReader a where
  getAssets :: Getting Assets a Assets

instance AssetsReader MainState where
  getAssets = assets

instance AssetsReader GameReader where
  getAssets = gameAssets

instance AssetsReader AgentReader where
  getAssets = agentAssets

class InputsReader a where
  getInputs :: Getting Inputs a Inputs

instance InputsReader MainState where
  getInputs = inputs

instance InputsReader AgentReader where
  getInputs = agentInputs

instance InputsReader GameReader where
  getInputs = gameInputs

class EnvironmentState a where
  environment :: Simple Lens a Environment

instance EnvironmentState GameState where
  environment = gameEnvironment

instance EnvironmentState MainState where
  environment = mainEnvironment

instance EnvironmentState (AgentState a) where
  environment = agentEnvironment

instance MonadTime GameMonad where
  getTimeMillis = use $ environment.currentTime

instance MonadTime MainMonad where
  getTimeMillis = liftIO $ fmap fromIntegral $ getTicks

instance MonadTime (AgentMonad a) where
  getTimeMillis = use $ environment.currentTime
