{-# LANGUAGE FlexibleInstances, MultiParamTypeClasses #-}
module XMonad.Layout.ZoomSecondary (ZoomSecondary(..), ZoomMessage(..), zoomSecondary) where

import XMonad
import XMonad.Layout.LayoutModifier

data ZoomSecondary a = ZoomSecondary Rational Bool
                       deriving (Read, Show)

data ZoomMessage = ZoomOn | ZoomOff | ZoomToggle

instance Message ZoomMessage

instance LayoutModifier ZoomSecondary a where
    pureModifier _ _ _ []  = ([], Nothing)
    pureModifier _ _ _ [w] = ([w], Nothing)
    pureModifier (ZoomSecondary _ False) _ _ ws = (ws, Nothing)
    pureModifier (ZoomSecondary amt True) r _ (w1:w2:ws) = (w1:zoom r w2:ws, Nothing)
        where
          zoom (Rectangle _ _ _ fullH) (a, Rectangle x y w h) = (a, Rectangle x y w h')
              where
                h' = max h $ floor $ fromIntegral fullH * amt

    pureMess (ZoomSecondary amt on) m = change <$> fromMessage m
        where
          change ZoomOn     = ZoomSecondary amt True
          change ZoomOff    = ZoomSecondary amt False
          change ZoomToggle = ZoomSecondary amt $ not on

zoomSecondary :: Rational -> Bool -> l a -> ModifiedLayout ZoomSecondary l a
zoomSecondary amount on = ModifiedLayout (ZoomSecondary amount on)
