import XMonad.Actions.CopyWindow
--
-- dodej več layoutuv na F1 F2 F3...
-- nared da pišejo v nek fajl npr echo Grid > status
-- genmon da dela cat status
--
--
-- named scratchpad for evince
-- test layout.minimize
-- rice gridselect
-- sublayout

import XMonad

import Data.Char
import Data.Monoid
import System.Exit

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.RotSlaves
import XMonad.Actions.Submap
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WindowGo
import XMonad.Actions.WithAll
-- deprecated
import XMonad.Actions.MouseGestures
import XMonad.Actions.GridSelect

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.SetWMName
-- test
import XMonad.Hooks.FadeWindows
--import XMonad.Hooks.MoreManageHelpers

import XMonad.Layout.Grid
import XMonad.Layout.Tabbed
import XMonad.Layout.ThreeColumns

import XMonad.Layout.Magnifier
import XMonad.Layout.Maximize
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import XMonad.Util.Run
import XMonad.Util.Scratchpad

import XMonad.Config.Xfce



-- GENERAL SETTINGS

setTerminal          = "urxvtc"
setModKey            = mod4Mask
setFocusFollowsMouse = True
setWorkspaces        = ["www","ii","iii","iv","v","vi","vii","viii","ix","NSP"] --NSP=scratchpad
setBorderWidth       = 1
setSpacing           = smartSpacing 5
--setColors            = [ "#b58900" -- focused border, current window
--                       , "#634102" -- unfocused border
--                       , "#121212" -- tab background
--                       , "#1f1f1f" -- tab foreground
--                       , "#dedede" -- inactive tab text color
--                       ]
setColors            = [ "#D33682" -- focused border
                       , "#93A1A1" -- unfocused border
                       --, "#073642" -- tab foreground
                       , "#002B36" -- tab foreground
                       , "#051A20" -- tab background
                       , "#2AA198" -- inactive tab text
                       ]
setTabFont           = "xft:sans:size=8"


-- MANAGE HOOKS

setManageHook = composeAll
        -- always float Gimp
        [ className =? "Gimp"               --> doFloat
        -- always float calendar
        , className =? "Orage"              --> doFloat
        -- always float R plots
        , className =? "R_x11"              --> doFloat
        -- always float Qjackctl
        , className =? "Qjackctl"           --> doFloat
        -- never open terminal as master
        , className =? "URxvt"              --> doF (W.swapDown)
        -- never open thunar as master
        , className =? "Thunar"              --> doF (W.swapDown)
        -- firefox always opens on workspace 1
        , className =? "Firefox"            --> doF (W.shift $ setWorkspaces !! 0)
        -- steam always opens on workspace 3
        , className =? "Steam"              --> doF (W.shift $ setWorkspaces !! 2)
        -- spring always opens on workspace 2
        , className =? "Springlobby"        --> doF (W.shift $ setWorkspaces !! 1)
        -- dota always opens on workspace 4
        , className =? "dota_linux"         --> doF (W.shift $ setWorkspaces !! 3)
        -- l4d2 always opens on workspace 4
        , className =? "hl2_linux"          --> doF (W.shift $ setWorkspaces !! 3)
        -- spring games open on workspace 4
        , className =? "spring"             --> doF (W.shift $ setWorkspaces !! 3)
        -- qemu almways opens on workspace 8
        , className =? "qemu-system-x86_64" --> doF (W.shift $ setWorkspaces !! 7)
        -- deluge always opens on workspace 9
        , className =? "Deluge"             --> doF (W.shift $ setWorkspaces !! 8)
        -- pidgin almways opens on workspace 9
        , className =? "Pidgin"             --> doF (W.shift $ setWorkspaces !! 8)
        -- eclipse
        , className =? "Eclipse"            --> doF (W.shift $ setWorkspaces !! 1)
        , className =? "Java"               --> doF (W.shift $ setWorkspaces !! 1)
        -- utoc
        , className =? "utox"               --> doF (W.shift $ setWorkspaces !! 7)
        -- ignore xfce4-notifyd
        , className =? "Xfce4-notifyd"      --> doIgnore
        ]
        <+> manageDocks
        <+> manageScratchpad
        <+> floatNextHook

manageScratchpad = scratchpadManageHook (W.RationalRect l t w h)
    where
        h = 0.30 -- height
        w = 0.60 -- width
        t = 0.30 -- distance from top edge, avoiding the taskbar
        l = 0    -- distance from left edge

-- LOGHOOK

setLogHook = ewmhDesktopsLogHookCustom scratchpadFilterOutWorkspace
          >> updatePointer (Relative 0.5 0.5)
--          >> composeAll [isUnfocused --> transparency 0.2
--                        ,                opaque
--                        ]

-- LAYOUTS AND LAYOUT HOOKS

-- default layout

tabbedL = named "Tabbed" $ tabbedBottom shrinkText tabbedTheme
    where
        tabbedTheme = defaultTheme { activeBorderColor   = setColors !! 0
                                   , inactiveBorderColor = setColors !! 1
                                   , activeColor         = setColors !! 2
                                   , inactiveColor       = setColors !! 3
                                   , activeTextColor     = setColors !! 0
                                   , inactiveTextColor   = setColors !! 1
                                   , fontName            = setTabFont
                                   }

threeCol = named "Three" $ ThreeCol 1 (3/100) (1/3)

-- default layout used on all workspaces except "www"
defLayout = toggleLayouts tabbedL (magnifierOff $ maximize $ setSpacing (tall ||| threeCol ||| Grid))
    where
        -- no. masters, increment, ratio
        tall = named "Tall" $ Tall 1 (1/100) (4/7)

-- layout used for workspace "www" - makes slave windows smaller
wwwLayout = toggleLayouts tabbedL (magnifierOff $ maximize (setSpacing (tall ||| threeCol ||| Grid)))
    where
        -- no. masters, increment, ratio
        tall = named "Tall" $ Tall 1 (1/100) (5/7)

-- layout hook
setLayoutHook = avoidStruts $ smartBorders $ onWorkspace (setWorkspaces !! 0) wwwLayout $
                defLayout


-- KEYBOARD SHORTCUTS

setKeys conf@(XConfig {XMonad.modMask = mod}) = M.fromList $

        -- GENERAL

        -- kill window
     -- [ ((mod                , xK_q),      kill)

        -- test PP->notifyd
        [ ((mod,                 xK_i),      dynamicLogString defaultPP >>= \d->spawn $"notify-send -t 1337 \"" ++ d ++ "\"")
        -- focus next window
        , ((mod                , xK_x),      windows W.focusDown)
        -- alt tab
        , ((mod1Mask           , xK_Tab),    windows W.focusDown)
        -- focus previous
        , ((mod                , xK_y),      windows W.focusUp)
        -- swap focused with next
        , ((mod .|. shiftMask  , xK_x),      rotSlavesDown)
        -- swap focused with prev
        , ((mod .|. shiftMask  , xK_y),      rotSlavesUp)
        -- focus master
        --, ((mod                , xK_m),      windows W.focusMaster)
        -- swap focused with master
        --, ((mod .|. shiftMask  , xK_m),      dwmpromote)
        -- swap focused with master
        , ((mod                 , xK_s),     dwmpromote)
        -- swap focused with master
        , ((mod                 , xK_s),     dwmpromote)
        -- focus master
        , ((mod .|. shiftMask   , xK_s),     windows W.focusMaster)
        -- shrink master
        , ((mod .|. controlMask, xK_y),      sendMessage Shrink)
        -- expand master
        , ((mod .|. controlMask, xK_x),      sendMessage Expand)
        -- magnifier toggle
        , ((mod                , xK_o),      sendMessage $ XMonad.Layout.Magnifier.Toggle)
        -- maximize toggle
        , ((mod .|. shiftMask  , xK_o),      withFocused $ sendMessage . maximizeRestore)
        -- float next window
        , ((mod                , xK_f),      toggleFloatNext)
        -- push window back into tiling
        , ((mod .|. shiftMask  , xK_t),      withFocused $ windows . W.sink)
        -- toggle panel 4
        , ((mod                , xK_less),   spawn "togglepanel4")
        -- toggle status bar
        , ((mod                , xK_b),      sendMessage ToggleStruts)
        -- quit x-session
        , ((mod .|. shiftMask  , xK_q),      spawn "xfce4-session-logout")
        -- mute speakers
        , ((mod .|. controlMask, xK_F1),     spawn ("amixer -c1 -q set PCM 0;" ++
                                                    "amixer -c1 -q set Master 100;" ++
                                                    "amixer -c1 -q set Headphone 100%"))
        -- unmute speakers
        , ((mod .|. controlMask, xK_F2),     spawn ("amixer -c1 -q set PCM 100%;" ++
                                                    "amixer -c1 -q set Master 100;" ++
                                                    "amixer -c1 -q set Headphone 0"))
        -- recompile, restart and show notification
        , ((mod .|. shiftMask  , xK_r),      spawn ("(xmonad --recompile && xmonad --restart &&" ++
                                                    "notify-send -t 1337 \"XMonad:\" \"Recompile successful.\") ||" ++
                                                    "notify-send -t 1337 \"XMonad:\" \"Recompile failed.\""))
        -- rotate through layout algorithms
        , ((mod .|. shiftMask  , xK_space),  sendMessage NextLayout)
        -- rotate through layout algorithms
        , ((mod .|. controlMask, xK_space),  sendMessage NextLayout >> sendMessage NextLayout)
        -- toggle tabbed layout
        , ((mod                , xK_space),  sendMessage $ XMonad.Layout.ToggleLayouts.Toggle "Tabbed")
        -- set tabbed
        , ((mod                , xK_F1),     sendMessage $ XMonad.Layout.ToggleLayouts.Toggle "Tabbed")
        -- set tall
        , ((mod                , xK_F2),     sendMessage FirstLayout)
        -- set three column
        , ((mod                , xK_F3),     sendMessage FirstLayout >> sendMessage NextLayout)
        -- set grid
        , ((mod                , xK_F4),     sendMessage FirstLayout >> sendMessage NextLayout >> sendMessage NextLayout)
        ] ++

        -- CHANGE WORKSPACES/MOVE BETWEEN WORKSPACES

        -- mod + shift + 1..9 to move to workspace
  --    [ ((m .|. mod, k), windows $ f i)
  --        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
  --        , (f, m) <- [(W.greedyView, 0), (\w -> W.greedyView w . W.shift w, shiftMask)]] ++
        --
        -- COPY WINDOW TEST
        [ ((m .|. mod, k), windows $ f i)
            | (i,k) <- zip (workspaces conf) [xK_1 ..]
            , (f,m) <- [(W.view, 0), (\w -> W.greedyView w . W.shift w, shiftMask), (copy, shiftMask .|. controlMask)]] ++

        -- swap workspaces
        [ ((mod .|. controlMask, k), windows $ swapWithCurrent i)
            | (i, k) <- zip setWorkspaces [xK_1..]] ++
        -- toggle WS
        [ ((mod                , xK_Tab),    toggleWS)
        -- next empty workspace a.k.a. show desktop
        , ((mod                , xK_d),      moveTo Next EmptyWS)
        -- next empty workspace a.k.a. show desktop
        , ((mod .|. shiftMask  , xK_d),      shiftTo Next EmptyWS)
        -- next screen
        , ((mod                , xK_w),      nextScreen)
        -- shift to next screen
        , ((mod .|. shiftMask  , xK_w),      shiftNextScreen)
        -- shift to next screen
        , ((mod .|. controlMask, xK_w),    swapNextScreen)
        -- next nonempty ws
        , ((mod .|. mod1Mask   , xK_x),      moveTo Next NonEmptyWS)
        -- next nonempty ws
        , ((mod .|. mod1Mask   , xK_y),      moveTo Prev NonEmptyWS)
        ] ++


        -- ARROW KEY/hjkl NAVIGATION
        [ ((mod, xK_Left),                   moveTo Prev NonEmptyWS)
        , ((mod, xK_Right),                  moveTo Next NonEmptyWS)
        , ((mod, xK_Up),                     windows W.focusUp)
        , ((mod, xK_Down),                   windows W.focusDown)
        -- move windows
        , ((mod .|. shiftMask, xK_Left),     shiftToPrev >> prevWS)
        , ((mod .|. shiftMask, xK_Right),    shiftToNext >> nextWS)
        , ((mod .|. shiftMask, xK_Up),       windows W.swapUp)
        , ((mod .|. shiftMask, xK_Down),     windows W.swapDown)
        -- hjkl
        , ((mod, xK_h),                      moveTo Prev NonEmptyWS)
        , ((mod, xK_l),                      moveTo Next NonEmptyWS)
        , ((mod, xK_k),                      windows W.focusUp)
        , ((mod, xK_j),                      windows W.focusDown)
        -- move windows
        , ((mod .|. shiftMask, xK_h),        shiftToPrev >> prevWS)
        , ((mod .|. shiftMask, xK_l),        shiftToNext >> nextWS)
        , ((mod .|. shiftMask, xK_k),        windows W.swapUp)
        , ((mod .|. shiftMask, xK_j),        windows W.swapDown)
        -- using c and v
        , ((mod, xK_c),                      moveTo Prev NonEmptyWS)
        , ((mod, xK_v),                      moveTo Next NonEmptyWS)
        , ((mod .|. shiftMask, xK_c),        shiftToPrev)
        , ((mod .|. shiftMask, xK_v),        shiftToNext)
        ] ++

        -- kill windows - qq to kill one, qa to kill all

        [ ((mod, xK_q), submap . M.fromList $
            -- quit all
            [ ((0,   xK_a), killAll)
            , ((mod, xK_a), killAll)
            , ((0,   xK_q), kill1)
            , ((mod, xK_q), kill)
            ])
        ] ++

        -- APPLICATION SHORTCUTS

        -- run
        [ ((mod, xK_r),                      spawn "gmrun")
        -- open thunar
        , ((mod, xK_e),                      spawn "thunar")
        -- open terminal
        , ((mod, xK_t),                      spawn $ "xrdb ~/.Xresources && " ++ setTerminal)
        -- scratchpad terminal
        , ((mod, xK_cedilla),                scratchpadSpawnActionCustom "tilda --name scratchpad")
        -- open task manager
        , ((mod, xK_Delete),                 spawn "xfce4-taskmanager")
        -- open pavucontrol
        , ((mod, xK_p),                      spawn "pavucontrol")
        -- goto or open programs with S-g
        , ((mod, xK_g), submap . M.fromList $
               -- firefox
               [ ((0, xK_f),      runOrRaise "firefox" (className =? "Firefox"))
               -- logisim
               , ((0, xK_l),      runOrRaise "logisim" (className =? "com-cburch-logisim-Main"))
               -- skype
               , ((0, xK_s),      runOrRaise "steam" (className =? "Steam"))
               -- mumble
               , ((0, xK_m),      runOrRaise "mumble" (className =? "Mumble"))
               -- task manager
               , ((0, xK_Delete), runOrRaise "xfce4-taskmanager" (className =? "Xfce4-taskmanager"))
               -- orange
               , ((0, xK_o),      runOrRaise "orange-canvas" (className =? "Orange-canvas"))
               -- deluge
               , ((0, xK_d),      runOrRaise "deluge" (className =? "Deluge"))
               -- terminal with vim
               , ((0, xK_v),      runInTerm setTerminal "vim")
               -- terminal with wifi-menu
               , ((0, xK_w),      runInTerm setTerminal "sudo wifi-menu && notify-send ")
               -- gridselect menu
               , ((0, xK_g),      goToSelected defaultGSConfig)
               -- calibre
               , ((0, xK_c),      runOrRaise "calibre" (className =? "Calibre-gui"))
               ])
        ]


-- MOUSE SHORTCUTS

-- mouse buttons are defined at the bottom of the file
setMouseBindings (XConfig {XMonad.modMask = mod}) = M.fromList $

        -- mod-l_button, set floating mode and move by dragging
        [ ((mod, mLeft),                     (\w -> focus w >> mouseMoveWindow w
                                                            >> windows W.shiftMaster))
        -- mod-r_button, resize window by dragging
        , ((mod .|. controlMask, mLeft),    (\w -> focus w >> mouseResizeWindow w
                                                           >> windows W.shiftMaster))
        -- mouse wheel to move between workspaces
        , ((mod, mWheelDn),                  (\_ -> moveTo Prev NonEmptyWS))
        , ((mod, mWheelUp),                  (\_ -> moveTo Next NonEmptyWS))
        -- mid button to close
        , ((mod, mMid),                      (\_ -> kill))
        -- gestures
        , ((mod, mRight),                    mouseGesture setGestures)
        ]


-- MOUSE GESTURES
setGestures = M.fromList
        -- focus follows mouse
        [ ([], focus)
        -- kill window
        , ([R, L, R], \w -> focus w >> kill)
        -- next workspace
        , ([R],       \_ -> moveTo Next NonEmptyWS)
        -- previous workspace
        , ([L],       \_ -> moveTo Prev NonEmptyWS)
        -- swap up
        , ([U],       \_ -> windows W.swapUp)
        -- swap down
        , ([D],       \_ -> windows W.swapDown)
        ]

-- MOUSE BUTTONS

mLeft    = 1 :: Button
mRight   = 3 :: Button
mMid     = 2 :: Button
mWheelDn = 4 :: Button
mWheelUp = 5 :: Button
mButton4 = 6 :: Button
mButton5 = 7 :: Button

-- MAIN FUNCTION

main = xmonad $ xfceConfig -- =<< statusBar setBar setPP setToggleKey xfceConfig
              { terminal           = setTerminal
              , modMask            = setModKey
              , keys               = setKeys
              , mouseBindings      = setMouseBindings
              , focusFollowsMouse  = setFocusFollowsMouse
              , borderWidth        = setBorderWidth
              , focusedBorderColor = setColors !! 0
              , normalBorderColor  = setColors !! 1
              , manageHook         = setManageHook
              , layoutHook         = setLayoutHook
              , logHook            = setLogHook
              --, handleEventHook    = fadeWindowsEventHook
              -- setWMName "LG3D" solves java compatibility
              , startupHook        = startupHook xfceConfig >> setWMName "LG3D"
              , workspaces         = setWorkspaces
              }


