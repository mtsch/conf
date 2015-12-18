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
import XMonad.Layout.Spacing
import XMonad.Layout.Tabbed
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ThreeColumns

import XMonad.ManageHook

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
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

              , borderWidth        = 1
              , focusedBorderColor = theColors !! 0
              , normalBorderColor  = theColors !! 1

              , manageHook         = theManageHook
              , layoutHook         = theLayoutHook
              , logHook            = theLogHook
              , startupHook        = startupHook xfceConfig
                                       >> setWMName "LG3D"

              , mouseBindings      = theMouseBindings
              , keys               = windowMovementKeys
              }
              `additionalKeysP` (keyMappings ++ shortcuts)
              --`additionalKeys`  windowMovementKeys

theTerminal   = "xrdb ~/.Xresources && urxvtc"
theSpacing    = smartSpacing 5
theWorkspaces = ["www","ii","iii","iv","v","vi","vii","viii","ix","NSP"]
theColors     = [ "#D33682" -- focused border
                , "#93A1A1" -- unfocused border
                --, "#073642" -- tab foreground
                , "#002B36" -- tab foreground
                , "#051A20" -- tab background
                , "#2AA198" -- inactive tab text
                ]
theTabFont    = "xft:sans:size=8"

-- directory with executable scripts
scriptDir = "/home/m/conf/scripts/"

theManageHook = composeAll
    -- never open as master
    [ className =? "URxvt"         --> doF (W.swapDown)
    , className =? "Thunar"        --> doF (W.swapDown)
    -- default workspaces
    , className =? "Firefox"       --> doF (W.shift $ theWorkspaces !! 0)
    , className =? "Steam"         --> doF (W.shift $ theWorkspaces !! 2)
    , className =? "dota_linux"    --> doF (W.shift $ theWorkspaces !! 3)
    -- don't focus xfce4-notifyd
    , className =? "Xfce4-notifyd" --> doIgnore
    ]
    <+> manageDocks
    <+> manageScratchpad

-- scratchpad thetings
manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.30 -- height
    w = 0.60 -- width
    t = 0.30 -- distance from top edge, avoiding the taskbar
    l = 0    -- distance from left edge

-- xfce integration, ignore scratchpad workspace, center mouse pointer on focus
theLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace
           >> updatePointer (Relative 0.5 0.5)

-- layouts
tabbedL = named "Tabbed" $ tabbedBottom shrinkText tabbedTheme
  where
    tabbedTheme = defaultTheme { activeBorderColor   = theColors !! 0
                               , inactiveBorderColor = theColors !! 1
                               , activeColor         = theColors !! 2
                               , inactiveColor       = theColors !! 3
                               , activeTextColor     = theColors !! 0
                               , inactiveTextColor   = theColors !! 1
                               , fontName            = theTabFont
                               }

threeCol = named "Three" $ ThreeCol 1 (3/100) (1/3)

-- create tall layout with ratio r
normalL r = toggleLayouts tabbedL (theSpacing (tall ||| threeCol ||| Grid))
  where
    tall = named "Tall" $ Tall 1 (1/100) (r)

-- default layout used on all workspaces except "www"
defaultLayout = normalL (4/7)
-- special "www" layout
wwwLayout     = normalL (5/7)

-- layout hook
theLayoutHook = avoidStruts
              $ smartBorders
              $ onWorkspace (theWorkspaces !! 0) wwwLayout
              $ defaultLayout

keyMappings =
    -- focus and window rotation
    [ ("M-x",       windows W.focusDown)
    , ("M1-<Tab>",  windows W.focusDown)
    , ("M-y",       windows W.focusUp)
    , ("M-S-x",     rotSlavesDown)
    , ("M-S-y",     rotSlavesUp)
    , ("M-s",       dwmpromote)
    , ("M-S-s",     windows W.focusMaster)
    , ("M-C-y",     sendMessage Shrink)
    , ("M-C-x",     sendMessage Expand)
    , ("M-b",       sendMessage ToggleStruts)
    , ("M-S-t",     withFocused $ windows . W.sink)
    -- workspaces
    , ("M-<Tab>",   toggleWS)
    , ("M-d",       moveTo Next EmptyWS)
    , ("M-S-d",     shiftTo Next EmptyWS)
    , ("M-w",       nextScreen)
    , ("M-S-w",     shiftNextScreen)
    , ("M-C-w",     swapNextScreen)
    , ("M-M1-x",    moveTo Next NonEmptyWS)
    , ("M-M1-y",    moveTo Prev NonEmptyWS)
    , ("M-c",       moveTo Prev NonEmptyWS)
    , ("M-v",       moveTo Next NonEmptyWS)
    , ("M-S-c",     shiftToPrev)
    , ("M-S-v",     shiftToNext)
    -- killing
    , ("M-q q",     kill1)
    , ("M-q M-q",   kill)
    , ("M-q a",     killAll)
    , ("M-q M-a",   killAll)
    -- layouts
    , ("M-<Space>", sendMessage $ Toggle "Tabbed")
    , ("M-<F1>",    sendMessage $ Toggle "Tabbed")
    , ("M-<F2>",    sendMessage FirstLayout)
    , ("M-<F3>",    sendMessage FirstLayout
                      >> sendMessage NextLayout)
    , ("M-<F4>",    sendMessage FirstLayout
                      >> sendMessage NextLayout
                      >> sendMessage NextLayout)
    -- misc
    , ("M-S-r",     spawn $ scriptDir ++ "recompile-xmonad")
    , ("M-i",       dynamicLogString defaultPP >>=
                        \d -> spawn $ "notify-send \"" ++ d ++ "\"")
    , ("M-S-q",     spawn "xfce4-session-logout")

    -- audio volume
    , ("<XF86AudioRaiseVolume>", spawn $ scriptDir ++ "pulse-volume '+'")
    , ("<XF86AudioLowerVolume>", spawn $ scriptDir ++ "pulse-volume '-'")
    , ("<XF86AudioMute>",        spawn $ scriptDir ++ "pulse-volume mute")
    ]

windowMovementKeys c@(XConfig {XMonad.modMask = mod}) = M.fromList $
    -- move windows between workspaces
    [ ((mod .|. controlMask, k), windows $ swapWithCurrent i)
        | (i, k) <- zip theWorkspaces [xK_1..]] ++

    -- copy windows between workspaces
    [ ((m .|. mod, k), windows $ f i)
        | (i, k) <- zip theWorkspaces [xK_1 ..]
        , (f, m) <- [ (W.view, 0)
                    , (\w -> W.greedyView w . W.shift w, shiftMask)
                    , (copy, shiftMask .|. controlMask)]] ++

    -- open scratchpad - cedilla not supported by EZConfig
    [ ((mod, xK_cedilla), scratchpadSpawnActionCustom
                            "urxvtc -name scratchpad") ]

shortcuts =
    [ ("M-r",      spawn "gmrun")
    , ("M-e",      spawn "thunar")
    , ("M-t",      spawn theTerminal)
    , ("M-<Del>",  spawn "xfce4-taskmanager")
    , ("M-p",      spawn "pavucontrol")
    , ("M-g f",    runOrRaise "firefox" $ className =? "Firefox")
    , ("M-g s",    runOrRaise "steam" $ className =? "Steam")
    , ("M-g m",    runOrRaise "mumble" $ className =? "Mumble")
    ]

theMouseBindings (XConfig {XMonad.modMask = mod}) = M.fromList $
    [ ((mod, 1 :: Button), (\w ->
          focus w >> mouseMoveWindow w >> windows W.shiftMaster))
    , ((mod .|. controlMask, 1 :: Button), (\w ->
          focus w >> mouseResizeWindow w >> windows W.shiftMaster))]
