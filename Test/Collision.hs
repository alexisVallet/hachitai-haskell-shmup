module Test.Collision (testCollision) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck
import Data.List hiding (insert)

import Collision
import Environment
import Test.ArbitraryInstances

testCollision =
  testGroup "Collision" [testProperty "same result as naive" pNaive]

naiveDetection :: AABB -> [AABB] -> [AABB]
naiveDetection aabb aabbs =
  filter (intersectAABB aabb) aabbs

pNaive :: AABB -> [AABB] -> Property
pNaive aabb aabbs =
  let 
    qtree = foldr insert (empty worldSize 4) aabbs
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
