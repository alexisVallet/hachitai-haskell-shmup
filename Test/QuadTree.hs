module Test.QuadTree (testQuadTree) where

import Test.Framework
import Test.Framework.Providers.QuickCheck2
import Test.QuickCheck

import Test.ArbitraryInstances
import QuadTree

testQuadTree = 
  testGroup "QuadTree" 
  [testProperty "insert" pInsert
  ,testProperty "remove" pRemove]

pInsert :: AABB -> QuadTree AABB -> Bool
pInsert aabb qTree =
  elem aabb $ toList $ insert aabb qTree

pRemove :: AABB -> QuadTree AABB -> Bool
pRemove aabb qTree =
  not $ elem aabb $ toList $ remove (==) aabb $ insert aabb qTree