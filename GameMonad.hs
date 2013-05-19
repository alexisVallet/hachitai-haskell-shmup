module GameMonad where

import Control.Lens
import Control.Monad.State
import Data.IORef

import Environment
import Assets
import Config

data Inputs = Inputs {
  _up :: Bool,
  _down :: Bool,
  _left :: Bool,
  _right :: Bool
  } deriving (Eq, Ord, Show)
makeLenses ''Inputs

type GameMonad = StateT GameState IO

data GameState = GameState {
  _environment :: Environment,
  _assets :: Assets,
  _inputs :: Inputs,
  _recurring :: [(Int,Int,GameMonad ())],
  _lifeRemaining :: Int,
  _delayed :: [(Int,GameMonad ())]
  }
makeLenses ''GameState

runGame :: GameMonad a -> IO a
runGame gameAction = do
  assets <- loadAssets
  let environment = initEnvironment assets
      inputs = Inputs False False False False
      recurring = []
      initialLifes = 3
      delayed = []
  fmap fst $ runStateT gameAction (GameState environment assets inputs recurring initialLifes delayed)

every :: Int -> GameMonad () -> GameMonad ()
every mseconds action = do
  recurring %= ((mseconds,0,action):)

delayedAction :: Int -> GameMonad () -> GameMonad ()
delayedAction mseconds action = do
  delayed %= ((mseconds,action):)

runRecurring :: GameMonad ()
runRecurring = do
  recurring' <- use recurring
  newRecurring <- forM recurring' $ \(rate,remaining,action) -> do
    if remaining - fromIntegral frameLength <= 0 then do
        action
        return (rate,rate,action)
      else return (rate, remaining - fromIntegral frameLength,action)
  recurring .= newRecurring

runDelayed :: GameMonad ()
runDelayed = do
  delayed' <- use delayed
  newDelayed <- liftIO $ newIORef []
  forM_ delayed' $ \(remaining,action) -> do
    if remaining - fromIntegral frameLength <= 0 
      then action
      else liftIO $ modifyIORef newDelayed ((remaining - fromIntegral frameLength,action):)
  newDelayed' <- liftIO $ readIORef newDelayed
  delayed .= newDelayed'