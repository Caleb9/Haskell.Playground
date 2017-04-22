module Geometry.Sphere
  ( area
  , volume
  ) where

area :: (Floating a) => a -> a
area r = 4 * pi * r^2

volume :: (Floating a) => a -> a
volume r = (4 * pi * r^3) / 3
