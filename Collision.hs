module Collision 
       (
         Quad.QuadTree,
         Quad.AABB(..),
         Quad.HasAABB(..),
         Quad.intersectAABB,
         Quad.empty,
         Quad.insert,
         Quad.toList,
         Quad.queryIntersecting,
         remove,
         move
       ) where

import Environment.Internal
import qualified QuadTree as Quad

remove :: (AgentClass a) => Int -> a -> Quad.QuadTree Agent -> Quad.QuadTree Agent
remove ident agent =
  Quad.remove (\a1 a2 -> getID a1 == getID a2) (Agent ident agent)

move :: (AgentClass a) => Int -> a -> a -> Quad.QuadTree Agent -> Quad.QuadTree Agent
move ident oldAgent newAgent =
  Quad.move 
  (\a1 a2 -> getID a1 == getID a2) 
  (Agent ident oldAgent)
  (Agent ident newAgent)