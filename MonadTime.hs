{-|
MonadTime is a class of monads for representing computations which can 
access the current time.
-}
module MonadTime 
       (
         MonadTime(..)
       ) where

import Control.Monad.IO.Class
import Graphics.UI.SDL.Time

class Monad m => MonadTime m where
  getTimeMillis :: m Int

instance MonadIO m => MonadTime m where
  getTimeMillis = liftIO $ fmap fromIntegral $ getTicks