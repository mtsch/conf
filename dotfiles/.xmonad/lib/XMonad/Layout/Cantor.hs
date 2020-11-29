{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.Cantor (Cantor(..), TallCantor(..), ResizeSecondary(..)) where

import Control.Monad
import Data.Ratio

import XMonad
import XMonad.Layout (splitHorizontallyBy)
import XMonad.StackSet (integrate)

-- TODO: Add the extra pixel to the top window
-- TODO: Add option to resize secondary window

data Cantor a = Cantor Rational Rational
                deriving (Read, Show)

data ResizeSecondary = ShrinkSecondary | ExpandSecondary
                     deriving Typeable
instance Message ResizeSecondary

instance LayoutClass Cantor a where
  pureLayout (Cantor _ ratio) rect stack = cantorTile ratio rect $ integrate stack

  pureMessage (Cantor delta ratio) m = resize <$> fromMessage m
    where
      resize Shrink = Cantor delta (max 0 $ ratio - delta)
      resize Expand = Cantor delta (min 1 $ ratio + delta)

  description _ = "Cantor"

data TallCantor a = TallCantor Rational Rational Rational Rational
                  deriving (Read, Show)

instance LayoutClass TallCantor a where
  pureLayout (TallCantor _ ratio _ sratio) rect stack = case integrate stack of
    []     -> []
    [w]    -> [(w, rect)]
    (w:ws) -> (w, left):cantorTile sratio right ws
      where
        (left, right) = splitHorizontallyBy ratio rect

  pureMessage (TallCantor delta ratio sdelta sratio) m =
            msum [ fmap resize    (fromMessage m)
                 , fmap resizeSec (fromMessage m)]
    where
      resize Shrink = TallCantor delta (max 0 $ ratio - delta) sdelta sratio
      resize Expand = TallCantor delta (min 1 $ ratio + delta) sdelta sratio
      resizeSec ShrinkSecondary = TallCantor delta ratio sdelta (max 0 $ sratio - sdelta)
      resizeSec ExpandSecondary = TallCantor delta ratio sdelta (min 1 $ sratio + sdelta)

  description _ = "TallCantor"

data RelativeRectangle = RelativeRectangle Rational Rational Rational Rational
                       deriving Show

transform :: Rectangle -> RelativeRectangle -> Rectangle
transform (Rectangle x y w h) (RelativeRectangle dx dy dw dh) = Rectangle x' y' w' h'
  where
    x' = floor $ toRational x + dx * toRational w
    y' = floor $ toRational y + dy * toRational h
    w' = floor $ dw * toRational w
    h' = floor $ dh * toRational h

cantor :: Rational -> Int -> [RelativeRectangle]
cantor ratio n = map getRect [1..n]
    where
      getRect 1 = RelativeRectangle (0 % 1) (0 % 1) (1 % 1) (if n == 1 then 1 % 1 else ratio)
      getRect i = RelativeRectangle x y w h
          where
            level = floor . logBase 2 $ fromIntegral i
            pos   = i - 2^level
            slotW = 1 % 2^level
            slotH = (1 % 2^level) * (1 - ratio)

            hasChildren = n >= 2 * i
            hasSibling = pos `rem` 2 == 1 || n > i

            x = fromIntegral pos * slotW
            y = 1 - 2 * slotH
            w = if hasSibling then slotW else 2 * slotW
            h = if hasChildren then slotH else 2 * slotH

cantorTile :: Rational -> Rectangle -> [a] -> [(a, Rectangle)]
cantorTile r rect ws = zip ws $ map (transform rect) $ cantor r $ length ws
