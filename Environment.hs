module Environment 
       (
         AABB(..),
         move,
         HasAABB(..),
         AgentType(..),
         AgentClass(..),
         Agent,
         Environment,
         GameMonad,
         MainMonad,
         agents,
         updateEnvironment,
         processInputs,
         Inputs,
         up,
         down,
         left,
         right,
         shot,
         laser,
         pause,
         newAgent,
         runGame,
         runMain,
         addAgent,
         toList
       ) where

import Control.Lens
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym

import Config
import Environment.Internal
import Collision

updateEnvironment :: Float -> GameMonad ()
updateEnvironment dt = do
  agents' <- use agents
  forM_ (toList agents') $ \(Agent ident agent) -> do
    live dt ident agent

processInputs :: MainMonad ()
processInputs = do
  event <- liftIO pollEvent
  case event of
    NoEvent -> return ()
    KeyDown keySym -> do
      changeInput keySym True
      processInputs
    KeyUp keySym -> do
      changeInput keySym False
      processInputs
    _ -> processInputs

changeInput :: Keysym -> Bool -> MainMonad ()
changeInput key value = do
  case symKey key of
    SDLK_UP -> up .= value
    SDLK_DOWN -> down .= value
    SDLK_RIGHT -> right .= value
    SDLK_LEFT -> left .= value
    SDLK_w -> shot .= value
    SDLK_x -> laser .= value
    SDLK_ESCAPE -> pause .= value
    _ -> return ()

uniqueID :: (Monad m) => EnvMonadT m Int
uniqueID = do
  newId <- use currentId
  currentId += 1
  return newId

newAgent :: (MonadTrans mt, Monad m, AgentClass a, Monad (mt (EnvMonadT m))) => a -> mt (EnvMonadT m) Agent
newAgent agent = do
  ident <- lift $ uniqueID
  return $ Agent ident agent

runGame :: GameMonad a -> MainMonad a
runGame gameAction = do
  inputs <- get
  environment <- lift $ get
  let (result,environment') = 
        runState (runReaderT gameAction inputs) environment
  lift $ put environment'
  return result

runMain :: MainMonad a -> IO a
runMain mainAction = do
  evalStateT (evalStateT mainAction initInputs) initEnv
  where
    initInputs = Inputs False False False False False False False
    initEnv = Environment (emptyQuadTree screenAABB qTreeHeight) 0

addAgent :: (Monad m) => Agent -> EnvMonadT m ()
addAgent agent = agents %= insertQTree agent
