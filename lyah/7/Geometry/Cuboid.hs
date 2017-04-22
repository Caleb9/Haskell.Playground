module Geometry.Cuboid
  ( area
  , volume
  ) where

area :: (Num a) => a -> a -> a -> a
area x y z = 2 * (x*y + y*z + z*x)

volume :: (Num a) => a -> a -> a -> a
volume x y z = x * y * z
