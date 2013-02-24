module Test.ArbitraryInstances 
       (
         worldSize, 
         treeDepth
       ) where

import Test.QuickCheck

import QuadTree

aabbArbitraryHelper :: AABB -> Gen AABB
aabbArbitraryHelper (AABB (wcx,wcy) (whx,why)) = do
  hx <- choose (1,whx)
  hy <- choose (1,why)
  cx <- choose (wcx-whx+hx,wcx+whx-hx)
  cy <- choose (wcy-why+hy,wcy+why-hy)
  return $ AABB (cx,cy) (hx,hy)

instance Arbitrary AABB where
  arbitrary = aabbArbitraryHelper worldSize

worldSize :: AABB
worldSize = AABB (320,240) (320,240)

treeDepth :: Int
treeDepth = 4

instance Arbitrary (QuadTree AABB) where
  arbitrary = arbitrary' worldSize treeDepth
    where arbitrary' size n 
            | n <= 0    = return Leaf
            | otherwise = do
              let (nwq,neq,swq,seq) = subQuadrants size
              nw <- arbitrary' nwq (n-1)
              ne <- arbitrary' neq (n-1)
              sw <- arbitrary' swq (n-1)
              se <- arbitrary' seq (n-1)
              content <- listOf $ aabbArbitraryHelper size
              return $ Node size content nw ne sw se
