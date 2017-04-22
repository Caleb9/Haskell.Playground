module Geometry.Cube
  ( area
  , volume
  ) where

import qualified Geometry.Cuboid as Cuboid

area :: (Num a) => a -> a
area x =  Cuboid.area x x x

volume :: (Num a) => a -> a
volume x = Cuboid.volume x x x
