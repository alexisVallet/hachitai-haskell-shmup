module Environment 
       (
         module Environment.Internal,
         AABB(..),
         HasAABB(..),
         AgentType(..),
         AgentClass(..),
         updateEnvironment,
         newAgent,
         runGame,
         runMain,
         addAgent
       ) where

import Control.Lens
import Control.Monad.Identity
import Control.Monad.State
import Control.Monad.Reader
import Control.Monad.RWS
import Graphics.UI.SDL.Events
import Graphics.UI.SDL.Keysym
import Graphics.UI.SDL.Time
import Data.Map as Map

import Config
import Environment.Internal
import Collision as Quad
import Assets.Internal
import MonadTime

updateEnvironment :: Float -> GameMonad ()
updateEnvironment dt = do
  agents' <- use $ environment.agents
  forM_ (Quad.toList agents') $ \(Agent ident oldAgent) -> do
    newAgent <- runAgent ident oldAgent $ do
      live dt
      use agent
    environment.agents %= move ident oldAgent newAgent

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
    SDLK_UP -> inputs.up .= value
    SDLK_DOWN -> inputs.down .= value
    SDLK_RIGHT -> inputs.right .= value
    SDLK_LEFT -> inputs.left .= value
    SDLK_w -> inputs.shot .= value
    SDLK_x -> inputs.laser .= value
    SDLK_ESCAPE -> inputs.pause .= value
    _ -> return ()

uniqueID :: (MonadState e m, EnvironmentState e) => m Int
uniqueID = do
  newId <- use $ environment.currentId
  environment.currentId += 1
  return newId

newAgent :: (MonadState e m, EnvironmentState e, AgentClass a) => a -> m Agent
newAgent agent = do
  ident <- uniqueID
  return $ Agent ident agent

runGame :: GameMonad a -> MainMonad a
runGame gameAction = do
  processInputs
  inputs' <- use inputs
  assets' <- use assets
  time <- getTimeMillis
  environment.currentTime .= time
  environment' <- use environment
  let 
    reader = GameReader inputs' assets'
    initGameState = GameState environment'
    (res,gameState,()) = runRWS gameAction reader initGameState
  environment .= gameState^.environment
  return res

runMain :: MainMonad a -> IO a
runMain mainAction = do
  time <- getTicks
  let
    initInputs = Inputs False False False False False False False
    initEnv = 
      Environment 
      (Quad.empty screenAABB qTreeHeight) 
      0
      (fromIntegral time)
    initAssets = Assets Map.empty
    initState = MainState initEnv initInputs initAssets
  evalStateT mainAction initState

runAgent :: (AgentClass a) => Int -> a -> AgentMonad a b -> GameMonad b
runAgent ident agent agentAction = do
  environment' <- use environment
  inputs' <- query getInputs
  assets' <- query getAssets
  let 
    initialState = AgentState environment' agent
    reader = AgentReader inputs' assets' ident
    (result,finalState,()) = runRWS agentAction reader initialState
  environment .= finalState^.environment
  return result

addAgent :: (MonadState e m, EnvironmentState e) => Agent -> m ()
addAgent agent = 
  environment.agents %= Quad.insert agent
