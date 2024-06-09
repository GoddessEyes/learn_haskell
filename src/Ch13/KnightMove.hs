module Ch13.KnightMove (moveKnight, in3, canReachIn3) where
  
import Control.Monad

type KnightPose = (Int, Int)

moveKnight :: KnightPose -> [KnightPose]
moveKnight (c, r) = do
  (c', r') <- [(c+2, r-1), (c+2, r+1), (c-2, r-1), (c-2, r+1), (c+1, r-2), (c+1,r+2), (c-1, r-2), (c-1, r+2)]
  guard (c' `elem` [1..8] && r' `elem` [1..8])
  return (c', r')
--
--in3 start = do
--  first <- moveKnight start
--  second <- moveKnight first
--  moveKnight second

in3 :: KnightPose -> [KnightPose]
in3 start = return start >>= moveKnight >>= moveKnight >>= moveKnight

canReachIn3 :: KnightPose -> KnightPose -> Bool
canReachIn3 start end = end `elem` in3 start
