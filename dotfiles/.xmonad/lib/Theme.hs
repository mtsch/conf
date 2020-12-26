module Theme where

import FancyXMobar
import System.IO
import XMonad.Util.WorkspaceCompare
import BlurWallpaper

type SColor = String

-- Solarized palette
base04 :: SColor
base04 = "#001e26"

base03 :: SColor
base03 = "#002b36"

base02 :: SColor
base02 = "#073642"

base01 :: SColor
base01 = "#586e75"

base00 :: SColor
base00 = "#657b83"

base0 :: SColor
base0 = "#839496"

base1 :: SColor
base1 = "#93a1a1"

base2 :: SColor
base2 = "#eee8d5"

base3 :: SColor
base3 = "#fdf6e3"

yellow :: SColor
yellow = "#b58900"

orange :: SColor
orange = "#cb4b16"

red :: SColor
red = "#dc322f"

magenta :: SColor
magenta = "#d33682"

violet :: SColor
violet = "#6c71c4"

blue :: SColor
blue = "#268bd2"

cyan :: SColor
cyan = "#2aa198"

green :: SColor
green = "#859900"


barBg = base04
underline' = underline 4

wallpapers = WallpaperSetup { mode    = "center"
                            , sharp   = "/home/m/conf/wallpapers/Fractal-Picture.jpg"
                            , blurred = "/home/m/conf/wallpapers/Fractal-Picture-blur.jpg"
                            }

-- fancy xmobar
thePP h = FancyPP { backgroundColor    = barBg
                  , sameSeparator      = underline' barBg ""
                  , diffSeparator      = underline' barBg ""
                  , separatorFont      = 3

                  , wsPrefix           = fancy barBg barBg " "
                  , tabWSSeparator     = fancy barBg barBg " "
                  , tabsPostfix        = fancy barBg barBg " "

                  , currentWSTheme     = fancy base03 base2 . font 2 . underline' blue
                  , visibleWSTheme     = fancy magenta base02 . font 2 . underline' barBg
                  , hiddenWSTheme      = fancy base1 base02 . font 2 . underline' barBg
                  , hiddenEmptyWSTheme = fancy base01 base04 . font 2 . underline' barBg
                  , urgentWSTheme      = fancy base03 red . font 2 . underline' barBg

                  , isTabbedLayout     = (== "t")
                  , tabAreaWidth       = 95
                  , noWindowsTheme     = fancy barBg barBg
                  , untabbedTheme      = fancy base01 barBg
                  , activeTabTheme     = fancy base1 base03 . underline' yellow
                  , inactiveTabTheme   = fancy base01 barBg

                  , wsSort             = getSortByIndex
                  , extraLoggers       = []
                  , outputHandler      = hPutStrLn h
                  }
