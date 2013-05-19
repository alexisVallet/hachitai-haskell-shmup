module Config where

import Data.Word

resolution :: (Int,Int)
resolution = (480, 640)

screenWidth :: Int
screenWidth = fst resolution

screenHeight :: Int
screenHeight = snd resolution

fps :: Word32
fps = 60

frameLength :: Word32
frameLength = 1000 `div` fps