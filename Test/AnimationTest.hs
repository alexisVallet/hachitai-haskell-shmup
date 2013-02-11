module Main (main) where

import Data.IORef
import Graphics.UI.SDL as SDL
import Animation
import Control.Monad
import Data.Maybe

fps :: Int
fps = 24

main :: IO ()
main = withInit [InitEverything] $ do
  screen <- setVideoMode 640 480 32 [SWSurface]
  animation <- fmap fromJust
               $ fmap (loopFrom 0) 
               $ fmap fromJust
               $ loadAnimation "./Test/animation" (1000`div`fps)
  animationPoint <- startAnimation animation >>= newIORef
  forM_ [1..500] $ \_ -> do
    frame <- fmap currentFrame $ readIORef animationPoint
    black <- mapRGB (surfaceGetPixelFormat screen) 0 0 0
    fillRect screen Nothing black
    blitSurface frame Nothing screen Nothing
    SDL.flip screen
    delay $ 1000`div`60
    readIORef animationPoint >>= nextFrame >>= writeIORef animationPoint
