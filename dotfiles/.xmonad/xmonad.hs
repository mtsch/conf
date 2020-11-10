import XMonad

import Data.Char
import Data.Monoid
import System.Exit

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.RotSlaves
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll

import XMonad.Config.Xfce

import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Layout.MouseResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ThreeColumns

import XMonad.ManageHook

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName

import XMonad.Util.Scratchpad
import XMonad.Util.EZConfig

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

main = xmonad $ xfceConfig
              { terminal           = theTerminal
              , modMask            = mod4Mask
              , focusFollowsMouse  = True
              , workspaces         = theWorkspaces

              , borderWidth        = 2
              , focusedBorderColor = focused theColors
              , normalBorderColor  = unfocused theColors

              , manageHook         = theManageHook
              , layoutHook         = theLayoutHook
              , logHook            = theLogHook
              , startupHook        = startupHook xfceConfig >> setWMName "LG3D"

              , mouseBindings      = theMouse
              , keys               = theWindowMovementKeys
              }
              `additionalKeysP` theKeys

theTerminal   = "xrdb ~/.Xresources && urxvtc"
theWorkspaces = ["i","ii","iii","iv","v","vi","vii","viii","ix","NSP"]

data ColorScheme = ColorScheme { focused    :: String
                               , unfocused  :: String
                               , background :: String
                               , foreground :: String
                               }
theColors = ColorScheme "#CB4B16" "#93A1A1" "#002B36" "#051A20"

theManageHook = composeAll
    -- never open as master
    [ className =? "URxvt"         --> doF (W.swapDown)
    , className =? "Thunar"        --> doF (W.swapDown)
    , className =? "Emacs"         --> doF (W.swapDown)
    -- default workspaces
    -- don't focus xfce4-notifyd
    , className =? "Xfce4-notifyd" --> doIgnore
    , className =? "Pavucontrol"   --> doRectFloat (W.RationalRect 0.69 0.10 0.30 0.40)
    , title     =? "Whisker Menu"  --> doFloat
    ]
    <+> manageDocks
    <+> scratchpadManageHook (W.RationalRect 0.01 0.30 0.50 0.50)
        -- left, top, width, height

-- xfce integration, ignore scratchpad workspace, center mouse pointer on focus
theLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace
           >> updatePointer (0.5, 0.5) (0, 0)

theLayoutHook = avoidStruts
              $ smartBorders
              $ toggleLayouts tabbed (tall1 ||| tall2 ||| tall3)
  where
    tall1 = named "1" $ mouseResizableTile { masterFrac = 4/7
                                           , draggerType = FixedDragger 10 10
                                           }
    tall2 = named "2" $ mouseResizableTile { masterFrac = 4/7
                                           , draggerType = FixedDragger 10 10
                                           , nmaster = 2
                                           }
    tall3 = named "3" $ mouseResizableTile { masterFrac = 4/7
                                           , draggerType = FixedDragger 10 10
                                           , nmaster = 3
                                           }
    tabbed = named "t" $ tabbedBottom shrinkText theme
      where
        theme = def { activeBorderColor   = focused theColors
                    , inactiveBorderColor = unfocused theColors
                    , activeColor         = background theColors
                    , inactiveColor       = foreground theColors
                    , activeTextColor     = unfocused theColors
                    , inactiveTextColor   = unfocused theColors
                    , fontName =
                        "xft:Ubuntu:weight=bold:pixelsize=12:antialias=true:hinting=true"
                    }

theKeys =
    -- focus and window rotation
    [ ("M-x",        windows W.focusDown)
    , ("M1-<Tab>",   windows W.focusDown)
    , ("M1-S-<Tab>", windows W.focusUp)
    , ("M-y",        windows W.focusUp)
    , ("M-z",        windows W.focusUp)
    , ("M-S-x",      rotSlavesDown)
    , ("M-S-y",      rotSlavesUp)
    , ("M-S-z",      rotSlavesUp)
    , ("M-s",        dwmpromote)
    , ("M-S-s",      windows W.focusMaster)
    , ("M-C-y",      sendMessage Shrink)
    , ("M-C-z",      sendMessage Shrink)
    , ("M-C-x",      sendMessage Expand)
    , ("M-b",        sendMessage ToggleStruts)
    , ("M-S-t",      withFocused $ windows . W.sink)
    , ("M-C-k",      sendMessage ShrinkSlave)
    , ("M-C-j",      sendMessage ExpandSlave)
    , ("M-C-h",      sendMessage Shrink)
    , ("M-C-l",      sendMessage Expand)
    -- workspaces
    , ("M-<Tab>",    toggleWS)
    , ("M-d",        moveTo Next EmptyWS)
    , ("M-S-d",      shiftTo Next EmptyWS)
    , ("M-w",        nextScreen)
    , ("M-S-w",      shiftNextScreen)
    , ("M-C-w",      swapNextScreen)
    , ("M-M1-x",     moveTo Next NonEmptyWS)
    , ("M-M1-y",     moveTo Prev NonEmptyWS)
    , ("M-M1-z",     moveTo Prev NonEmptyWS)
    , ("M-c",        moveTo Prev NonEmptyWS)
    , ("M-v",        moveTo Next NonEmptyWS)
    , ("M-S-c",      shiftToPrev)
    , ("M-S-v",      shiftToNext)
    -- killing
    , ("M-q q",      kill1)
    , ("M-q M-q",    kill)
    , ("M-q a",      killAll)
    , ("M-q M-a",    killAll)
    -- layouts
    , ("M-<Space>",  sendMessage $ Toggle "t")
    , ("M-<F1>",     sendMessage FirstLayout)
    , ("M-<F2>",     sendMessage FirstLayout >>
                         sendMessage NextLayout)
    , ("M-<F3>",     sendMessage FirstLayout >>
                         sendMessage NextLayout >>
                         sendMessage NextLayout)
    -- misc
    , ("M-i",          dynamicLogString def >>= \d -> spawn $ "notify-send \"" ++ d ++ "\"")
    , ("M-S-q",        spawn "xfce4-session-logout")
    , ("M-`",          scratchpadSpawnActionTerminal "urxvtc")
    -- application shortcuts
    , ("M-r",          spawn "xfce4-popup-whiskermenu")
    , ("M-e",          spawn "thunar")
    , ("M-t",          spawn theTerminal)
    , ("M-S-<Delete>", spawn "xfce4-taskmanager")
    , ("M-<Delete>",   spawn "urxvtc -e htop")
    , ("M-p",          spawn "pavucontrol")
    , ("M-m",          spawn "emacsclient -c")
    , ("M-g f",        runOrRaise "firefox" $ className =? "firefox")
    ]

theWindowMovementKeys c@(XConfig {XMonad.modMask = mod}) = M.fromList $
    -- move windows between workspaces
    [ ((mod .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip theWorkspaces [xK_1..]] ++

    -- copy windows between workspaces
    [ ((m .|. mod, k), windows $ f i)
        | (i, k) <- zip theWorkspaces [xK_1 ..]
        , (f, m) <- [ (W.view, 0)
                    , (\w -> W.greedyView w . W.shift w, shiftMask)
                    , (copy, shiftMask .|. controlMask)]]


theMouse (XConfig {XMonad.modMask = mod}) = M.fromList $
    [ ((mod, 1 :: Button), (\w ->
          focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod .|. controlMask, 1 :: Button), (\w ->
          focus w >> mouseResizeWindow w >> windows W.shiftMaster))]
