module Config where

import QuadTree
import Data.Word

framerate :: Int
framerate = 60

frameSeconds :: Float
frameSeconds = 1/realToFrac framerate

frameTicks :: Word32
frameTicks = 1000`div`fromIntegral framerate

screenAABB :: AABB
screenAABB =
  let halfSize = (240, 340) in
  AABB halfSize halfSize

screenWidth :: Int
screenWidth =
  let AABB _ (hw,_) = screenAABB in
  2 * round hw

screenHeight :: Int
screenHeight =
  let AABB _ (_,hh) = screenAABB in
  2 * round hh

screenCenter :: (Float,Float)
screenCenter =
  let AABB center _ = screenAABB in
  center

qTreeHeight :: Int
qTreeHeight = 4
