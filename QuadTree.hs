module QuadTree
       (
         QuadTree(..),
         AABB(..),
         HasAABB(..),
         subQuadrants,
         intersectAABB,
         empty,
         insert,
         remove,
         move,
         toList,
         queryIntersecting
       ) where

import Data.List hiding (insert)
import Control.Exception

-- | AABB with center and half dimension
data AABB = AABB (Float,Float) (Float,Float)
            deriving (Eq, Ord, Show)

class HasAABB a where
  getAABB :: a -> AABB

instance HasAABB AABB where
  getAABB = id

intersectAABB :: AABB -> AABB -> Bool
intersectAABB (AABB (c1x,c1y) (h1x,h1y)) (AABB (c2x,c2y) (h2x,h2y)) =
  intersect1D (c1x-h1x) (c1x+h1x) (c2x-h2x) (c2x+h2x)
  && intersect1D (c1y-h1y) (c1y+h1y) (c2y-h2y) (c2y+h2y)
  where intersect1D x1 x2 y1 y2 = not $ x2 < y1 || x1 > y2

data Quadrant = NW | NE | SW | SE

-- | QuadTree data structure containing AABBs
data QuadTree a = Leaf
                | Node AABB [a] (QuadTree a) (QuadTree a)
                                (QuadTree a) (QuadTree a)
                  deriving (Eq, Ord, Show)

subQuadrants :: AABB -> (AABB,AABB,AABB,AABB)
subQuadrants (AABB (cx,cy) (hx,hy)) =
  let newHX = hx/2 
      newHY = hy/2 
      newHalfSize = (newHX, newHY) in
  (AABB (cx-newHX,cy-newHY) newHalfSize,
   AABB (cx+newHX,cy-newHY) newHalfSize,
   AABB (cx-newHX,cy+newHY) newHalfSize,
   AABB (cx+newHX,cy+newHY) newHalfSize)

empty :: AABB -> Int -> QuadTree a
empty worldSize height
  | height <= 0 = Leaf
  | otherwise   = Node worldSize [] (child nw) (child ne) 
                                    (child sw) (child se)
  where
    (nw,ne,sw,se) = subQuadrants worldSize
    child quadrant = empty quadrant (height-1)

intersectingChildren :: AABB -> QuadTree a -> [QuadTree a]
intersectingChildren xAABB (Node _ _ nw ne sw se) = 
  map snd $
  filter fst $
  [(intersectAABB qAABB xAABB, quadrant) 
   | quadrant@(Node qAABB _ _ _ _ _) <- [nw,ne,sw,se]]

intersectingQuadrants :: AABB -> QuadTree a -> [Quadrant]
intersectingQuadrants aabb (Node _ _ nw ne sw se) =
  map snd $
  filter fst $
  [(intersectAABB qAABB aabb, quadrant) 
   | ((Node qAABB _ _ _ _ _),quadrant) <- [(nw,NW),(ne,NE),(sw,SW),(se,SE)]]

insert :: HasAABB a => a -> QuadTree a -> QuadTree a
insert x (Node size xs Leaf _ _ _) =
  Node size (x:xs) Leaf Leaf Leaf Leaf
insert x node@(Node size xs nw ne sw se) =
  let
    xAABB = getAABB x in
  case intersectingQuadrants xAABB node of
    [NW] -> Node size xs (insert x nw) ne sw se
    [NE] -> Node size xs nw (insert x ne) sw se
    [SW] -> Node size xs nw ne (insert x sw) se
    [SE] -> Node size xs nw ne sw (insert x se)
    _ -> Node size (x:xs) nw ne sw se

queryClose :: (HasAABB a) => AABB -> QuadTree a -> [a]
queryClose _ (Node size bs Leaf _ _ _) = bs
queryClose aabb node@(Node size bs _ _ _ _) =
  bs ++ concatMap (queryClose aabb) (intersectingChildren aabb node)

queryIntersecting :: (HasAABB a, HasAABB b) => a -> QuadTree b -> [b]
queryIntersecting a qtree =
  let aabb = getAABB a in
  filter (intersectAABB aabb . getAABB) $ queryClose aabb qtree

remove :: (HasAABB a) => (a -> a -> Bool) -> a -> QuadTree a -> QuadTree a
remove eqPred agent (Node size xs Leaf _ _ _) =
  Node size (deleteBy eqPred agent xs) Leaf Leaf Leaf Leaf
remove eqPred agent node@(Node size xs nw ne sw se) =
  case intersectingQuadrants (getAABB agent) node of
    [NW] -> Node size xs (remove eqPred agent nw) ne sw se
    [NE] -> Node size xs nw (remove eqPred agent ne) sw se
    [SW] -> Node size xs nw ne (remove eqPred agent sw) se
    [SE] -> Node size xs nw ne sw (remove eqPred agent se)
    _ -> Node size (deleteBy eqPred agent xs) nw ne sw se
remove _ _ _ = error "cannot remove object from a leaf!"

move :: (HasAABB a) => (a -> a -> Bool) -> a -> a -> QuadTree a -> QuadTree a
move eqPred oldAgent newAgent qtree =
  assert (eqPred oldAgent newAgent)
  $ insert newAgent $ remove eqPred oldAgent qtree

toList :: QuadTree a -> [a]
toList Leaf = []
toList (Node _ xs nw ne sw se) = 
  xs ++ toList nw ++ toList ne ++ toList sw ++ toList se
