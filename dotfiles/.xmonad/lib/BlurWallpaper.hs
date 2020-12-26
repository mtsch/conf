module BlurWallpaper (WallpaperSetup(..), blurWallpaper) where

import Control.Monad.Extra
import Data.Maybe

import XMonad
import qualified XMonad.StackSet as S
import qualified XMonad.Util.ExtensibleState as XS

data WallpaperSetup = WallpaperSetup { mode    :: String
                                     , sharp   :: String
                                     , blurred :: String
                                     }
data BlurState = Blurred | Sharp | Unknown
  deriving (Eq, Show)

instance ExtensionClass BlurState where
  initialValue = Unknown

blurWallpaper :: WallpaperSetup -> X ()
blurWallpaper (WallpaperSetup mode sharp blurred) = do
  hasWindows <- isJust . S.peek <$> gets windowset
  let wantState = if hasWindows then Blurred else Sharp
  whenM (XS.modified (const wantState)) $
    case wantState of
      Sharp   -> spawn $ "xwallpaper --" ++ mode ++ " " ++ sharp
      Blurred -> spawn $ "xwallpaper --" ++ mode ++ " " ++ blurred
      --Sharp -> spawn $ "notify-send sharp"
      --Blurred -> spawn $ "notify-send blurred"
