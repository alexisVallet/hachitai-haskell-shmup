module Animation where

import Graphics.UI.SDL
import Graphics.UI.SDL.Image
import Control.Monad

type Animation = [Surface]

loadAnimation :: FilePath -> Int -> IO Animation
loadAnimation folder size = do
  frames <- forM [0..size-1] $ \i -> do
    load $ folder ++ show i ++ ".png"
  return $ cycle frames