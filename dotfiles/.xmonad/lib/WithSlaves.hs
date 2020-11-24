module WithSlaves (withSlaves, killSlaves) where

import XMonad
import XMonad.StackSet

import Data.Foldable

withSlaves :: (Window -> X ()) -> X ()
withSlaves f = withWindowSet $ \ws -> f' . integrate' . stack . workspace . current $ ws
    where
      f' []     = return ()
      f' (w:ws) = forM_ ws f

killSlaves :: X ()
killSlaves = withSlaves killWindow
