module Rendering where

import Control.Lens

import Environment

renderEnvironment :: Surface -> Environment -> IO ()
renderEnvironment screen env = do
  let renderInfo = 
        sortBy (\(_,_,l1) (_,_,l2) -> compare l1 l2)
        $ map render
        $ toList
        $ env^.agents
  forM_ renderInfo $ \(surface,rect,_) -> do
    blitSurface screen Nothing surface (Just rect)