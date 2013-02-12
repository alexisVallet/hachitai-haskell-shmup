module Main (main) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Collision
import Data.List

instance Arbitrary AABB where
  arbitrary = do
    let AABB (wcx,wcy) (whx,why) = worldSize
    cx <- choose (wcx-whx,wcx+whx)
    cy <- choose (wcy-why,wcy+why)
    hx <- choose (1,whx)
    hy <- choose (1,why)
    return $ AABB (cx,cy) (hx,hy)

worldSize :: AABB
worldSize = AABB (320,240) (320,240)

main :: IO ()
main =
  defaultMain
  [testProperty "same result as naive" pNaive]

naiveDetection :: AABB -> [AABB] -> [AABB]
naiveDetection aabb aabbs =
  filter (intersectAABB aabb) aabbs

pNaive :: AABB -> [AABB] -> Property
pNaive aabb aabbs =
  let 
    qtree = foldr insertQTree (emptyQuadTree worldSize 4) aabbs
    naiveResult = sort $ naiveDetection aabb aabbs
    qtreeResult =
      sort
      $ queryIntersecting aabb qtree
    showResults = do
      putStrLn $ "QuadTree: " ++ show qtree
      putStrLn $ "Expected: " ++ show naiveResult ++ ", actual: " ++ show qtreeResult
  in
  whenFail showResults
  $ naiveResult == qtreeResult