module Assets.Internal
       (
         Assets(..),
         animations
       ) where

import Control.Lens
import Data.Map

import Animation

data Assets = Assets {
  _animations :: Map String Animation
  }
makeLenses ''Assets
