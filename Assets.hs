module Assets 
       (
         module Assets.Internal,
         addAnimation,
         getAnimation
       ) where

import Environment
import Assets.Internal
import Animation

import Data.Map as Map
import Control.Lens
import Control.Monad.IO.Class
import Control.Monad.Trans
import Control.Monad.Reader.Class

addAnimation :: String -> Animation -> MainMonad ()
addAnimation animationName animation =
  assets.animations %= insert animationName animation

getAnimation :: (MonadReader a m, AssetsReader a) => String -> m (Maybe Animation)
getAnimation animationName = do
  assets' <- query getAssets
  return $ Map.lookup animationName (assets'^.animations)
