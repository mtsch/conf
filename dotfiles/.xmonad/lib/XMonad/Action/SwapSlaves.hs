module XMonad.Action.SwapSlaves where

import XMonad
import XMonad.StackSet

reverseSlaves :: Stack a -> Stack a
reverseSlaves (Stack t (m:ls) rs) = Stack t (m:rs) ls
reverseSlaves (Stack t [] rs) = Stack t [] rs

swapSlavesUp = modify' swapSlavesUp'
swapSlavesDown = modify' (reverseSlaves . swapSlavesUp' . reverseSlaves)

swapSlavesUp' :: Stack a -> Stack a
swapSlavesUp' (Stack m [] rs) = Stack m [] rs
swapSlavesUp' (Stack t [m] rs) = Stack t (reverse (m:rs)) []
swapSlavesUp' (Stack t (l:ls) rs) = Stack t ls (l:rs)

dwmPromoteSlaves = windows $ modify' dwmPromoteSlaves'

dwmPromoteSlaves' :: Stack a -> Stack a
-- Master selected - swap top two slaves
dwmPromoteSlaves' (Stack m [] []) = Stack m [] []
dwmPromoteSlaves' (Stack m [] [r]) = Stack m [] [r]
dwmPromoteSlaves' (Stack m [] (r1:r2:rs)) = Stack m [] (r2:r1:rs)
-- Top slave selected
dwmPromoteSlaves' (Stack t [m] []) = Stack t [m] []
dwmPromoteSlaves' (Stack t [m] (r:rs)) = Stack r [m] (t:rs)
-- General case
dwmPromoteSlaves' (Stack t ls rs) = Stack t [m] rs'
  where
    m:l:rest = reverse ls
    rs' = rest ++ [l] ++ rs
