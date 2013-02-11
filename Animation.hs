module Animation 
       (
         Animation,
         AnimationPoint,
         loadAnimation,
         loopFrom,
         startAnimation,
         nextFrame,
         currentFrame
       ) where

import Graphics.UI.SDL hiding (flip)
import Graphics.UI.SDL.Image
import System.Directory
import Data.IORef
import Data.List
import Control.Monad

import MonadTime

data Animation = Animation [Surface] Int Int Int

data AnimationPoint = AnimationPoint [Surface] Int Int

-- | Loads an animation from a folder of images. The frames of the 
-- animation will be loaded in lexical (alphabetical) order of their
-- filename. The images must be in PNG format. Fails when the folder
-- contains no PNG images, or when the specified speed is negative.
loadAnimation :: FilePath -> Int -> IO (Maybe Animation)
loadAnimation folderPath speed = do
  dirContents <- getDirectoryContents folderPath
  animation <- newIORef []
  putStrLn $ show dirContents
  forM_ [0..length dirContents-1] $ \i -> do
    let fullPath = folderPath ++ "/" ++ show i ++ ".png"
    exists <- doesFileExist fullPath
    when exists $ do    
      frame <- load fullPath
      modifyIORef animation (frame:)
  frames <- readIORef animation
  let size = length frames
  if null frames || speed <= 0
    then return Nothing
    else return $ Just $ Animation frames speed (size - 1) size

-- | Loops an animation indefinitely starting from a specific frame. Fails 
-- if the frame index is not in the interval [0; length animation-1].
loopFrom :: Int -> Animation -> Maybe Animation
loopFrom loopStart (Animation frames speed _ size) =
  if loopStart >= size
  then Nothing
  else Just $ Animation frames speed loopStart size

startAnimation :: MonadTime m => Animation -> m AnimationPoint
startAnimation (Animation frames speed loopStart _) = do
  let (start,loop) = splitAt loopStart frames
  currentTicks <- getTimeMillis
  return $ AnimationPoint (start ++ cycle loop) speed currentTicks

nextFrame :: MonadTime m => AnimationPoint -> m AnimationPoint
nextFrame ap@(AnimationPoint (x:[]) _ _) = return ap
nextFrame ap@(AnimationPoint (x:xs) ticksPerFrame lastChange) = do
  currentTicks <- getTimeMillis
  let dt = currentTicks - lastChange
  if dt > ticksPerFrame
    then return $ AnimationPoint xs ticksPerFrame currentTicks
    else return ap
nextFrame _ = error "empty animation point, shouldn't happen"

currentFrame :: AnimationPoint -> Surface
currentFrame (AnimationPoint (x:_) _ _) = x
currentFrame _ = error "empty animation point, shouldn't happen"
