module Rendering where

import Control.Lens
import Graphics.UI.SDL as SDL
import Data.List
import Control.Monad

import Environment
import Collision

renderEnvironment :: Surface -> Environment -> IO ()
renderEnvironment screen env = do
  let renderInfo = 
        sortBy (\(_,_,l1) (_,_,l2) -> compare l1 l2)
        $ map (\(Agent _ a) -> render a)
        $ toList
        $ env^.agents
  black <- mapRGB (surfaceGetPixelFormat screen) 0 0 0
  fillRect screen Nothing black
  forM_ renderInfo $ \(surface,rect,_) -> do
    blitSurface surface Nothing screen (Just rect)
  SDL.flip screen