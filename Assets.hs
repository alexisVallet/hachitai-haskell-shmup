module Assets where

import Data.Map hiding (foldr)
import Graphics.UI.SDL.Image
import Control.Lens
import Control.Monad

import Animation

assetsPath :: String
assetsPath = "./assets/"

data Assets = Assets {
  _bulletAnimations :: Map String Animation,
  _shipAnimations :: Map String Animation,
  _counterAnimations :: Map String Animation,
  _gameOverAnimations :: Map String Animation
  } deriving (Eq, Ord, Show)
makeLenses ''Assets

loadAssets :: IO Assets
loadAssets = do
  let bulletAnimationFolder = assetsPath ++ "bullet/"
      shipAnimationFolder = assetsPath ++ "ship/"
      counterAnimationFolder = assetsPath ++ "lifecounter/"
      gameOverFolder = assetsPath ++ "gameover/"
      bulletAnimationNames = [("neutral", 1), ("blue", 1)]
      shipAnimationNames = [("neutral", 1), ("left", 1), ("right", 1)]
      counterAnimationNames = [("neutral",1)]
      gameOverAnimationNames = [("neutral",1)]
  bulletAnimations <- loadAnimations bulletAnimationFolder bulletAnimationNames
  shipAnimations <- loadAnimations shipAnimationFolder shipAnimationNames
  counterAnimations <- loadAnimations counterAnimationFolder counterAnimationNames
  gameOverAnimations <- loadAnimations gameOverFolder gameOverAnimationNames
  return $ Assets bulletAnimations shipAnimations counterAnimations gameOverAnimations

loadAnimations :: FilePath -> [(String,Int)] -> IO (Map String Animation)
loadAnimations folder animationNames = do
  animations <- forM animationNames $ \(name, size) -> do
    animation <- loadAnimation (folder ++ name ++ "/") size
    return (name, animation)
  return $ foldr (\(name, animation) animMap -> insert name animation animMap) empty animations
