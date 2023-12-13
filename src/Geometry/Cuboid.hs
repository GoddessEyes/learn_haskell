module Geometry.Cuboid
  ( volume,
    area,
  )
where

volume :: Float -> Float -> Float -> Float
volume a b c = rectArea a b * c

area :: Float -> Float -> Float -> Float
area a b c = a `rectArea` b * 2 + a `rectArea` c * 2 + c `rectArea` b * 2

rectArea :: Float -> Float -> Float
a `rectArea` b = a * b
