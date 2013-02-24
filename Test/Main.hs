module Main (main) where

import Test.Framework

import Test.Collision
import Test.QuadTree

main :: IO ()
main = defaultMain
       [testCollision,
        testQuadTree]

