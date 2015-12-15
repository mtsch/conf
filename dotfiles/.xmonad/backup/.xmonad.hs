import XMonad

import Data.Monoid
import System.Exit
import XMonad.Hooks.DynamicLog

import XMonad.Layout.PerWorkspace
import XMonad.Layout.Named
import XMonad.Layout.Maximize

import XMonad.Layout.Grid
import XMonad.Layout.ToggleLayouts

import XMonad.Actions.MouseGestures
import XMonad.Actions.CycleWS
import XMonad.Actions.Submap
import XMonad.Actions.WindowGo
import XMonad.Actions.UpdatePointer

import qualified Data.Map        as M
import qualified XMonad.StackSet as W

import Data.Char

import XMonad.Config.Xfce
import XMonad.Hooks.ManageDocks

-- GENERAL SETTINGS

setTerminal          = "urxvtc"
setModKey            = mod4Mask
setFocusFollowsMouse = True
setWorkspaces        = ["www","ii","iii","iv","v","vi","vii","viii","ix"]
setBorderWidth       = 2
setColors            = [ "#9D4C1B" -- focused, current window
                       , "#351907" -- unfocused border
                       , "#ffffff" -- hidden window
                       ]

-- MANAGE HOOKS

setManageHook = composeAll
        -- always float Gimp
        [ className =? "Gimp"     --> doFloat
        -- always float R plots
        , className =? "R_x11"    --> doFloat
        -- never open terminal as master
        , className =? "URxvt"    --> doF (W.swapDown)
        -- firefox always opens on workspace 1
        , className =? "Firefox"  --> doF (W.shift $ setWorkspaces !! 0)
        -- deluge always opens on workspace 9
        , className =? "Deluge"   --> doF (W.shift $ setWorkspaces !! 8)
        ]

-- LOGHOOK

setLogHook = updatePointer (Relative 0.5 0.5)


-- LAYOUTS AND LAYOUT HOOKS

-- default layout
defLayout = toggleLayouts full (tiled ||| grid)
    where
        -- no. masters, increment, ratio
        tiled = named "T" $ Tall 1 (3/100) (2/(1+(toRational(sqrt(5)::Double))))
        -- fullscreen
        full  = named "F" $ Full
        -- grid
        grid  = named "G" $ Grid

-- layout used for workspace 1 - makes slave windows smaller
wwwLayout = toggleLayouts full (tiled ||| grid)
    where
        -- no. masters, increment, ratio
        tiled = named "T" $ Tall 1 (3/100) (3/4) 
        -- fullscreen
        full  = named "F" $ Full
        -- grid
        grid  = named "G" $ Grid
    
-- layout hook
setLayoutHook = avoidStruts $ onWorkspace (setWorkspaces !! 0) wwwLayout $
                defLayout


-- KEYBOARD SHORTCUTS

setKeys conf@(XConfig {XMonad.modMask = mod}) = M.fromList $
	
        -- GENERAL

        -- kill window
        [ ((mod                , xK_q),      kill)
        -- toggle full layout
        , ((mod                , xK_space),  sendMessage $ Toggle "F") 
        -- rotate through layout algorithms
        , ((mod .|. shiftMask  , xK_space),  sendMessage NextLayout)
        -- focus next window
        , ((mod                , xK_x),      windows W.focusDown)
        -- focus previous
        , ((mod                , xK_y),      windows W.focusUp)
        -- focus master
        , ((mod                , xK_m),      windows W.focusMaster)
        -- swap focused with next
        , ((mod .|. shiftMask  , xK_x),      windows W.swapDown)
        -- swap focused with prev
        , ((mod .|. shiftMask  , xK_y),      windows W.swapUp)
        -- swap focused with master
        , ((mod .|. shiftMask  , xK_m),      windows W.swapMaster)
        -- shrink master
        , ((mod .|. controlMask, xK_y),      sendMessage Shrink)
        -- expand master
        , ((mod .|. controlMask, xK_x),      sendMessage Expand)
        -- push window back into tiling
        , ((mod .|. shiftMask  , xK_t),      withFocused $ windows . W.sink)
        -- quit xmonad
        , ((mod .|. shiftMask  , xK_q),      spawn "xfce4-session-logout")
        -- restart xmonad
        , ((mod .|. shiftMask  , xK_r),      spawn "xmonad --recompile; xmonad --restart")
        ] ++

        -- CHANGE WORKSPACES/MOVE BETWEEN WORKSPACES
	    
        -- mod + (shift) + 1..9
        [ ((m .|. mod, k), windows $ f i)
	    | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
	    , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]
        ] ++
        -- toggle WS
        [ ((mod                , xK_Tab),    toggleWS)
        -- next empty workspace a.k.a. show desktop
        , ((mod                , xK_d),      moveTo Next EmptyWS)
        -- next screen
        , ((mod                , xK_s),      swapNextScreen)
        -- shift to next screen
        , ((mod .|. shiftMask  , xK_s),      shiftNextScreen)
        ] ++

        -- APPLICATION SHORTCUTS
        
        -- run
        [ ((mod                , xK_r),      spawn "gmrun")
        -- open thunar
        , ((mod                , xK_e),      spawn "thunar")
        -- open terminal
        , ((mod                , xK_t),      spawn setTerminal)
        -- open task manager
        , ((mod                , xK_Delete), spawn "xfce4-taskmanager")
        -- open pavucontrol
        , ((mod                , xK_p),      spawn "pavucontrol")
        -- goto or open programs with S-g
        , ((mod,   xK_g), submap . M.fromList $
               -- firefox
               [ ((0, xK_f),      runOrRaise "firefox" (className =? "Firefox"))
               -- logisim
               , ((0, xK_l),      spawn      "java -jar ~/jar/logisim-generic-2.7.1.jar")
               -- skype
               , ((0, xK_s),      runOrRaise "steam" (className =? "Steam"))
               -- mumble
               , ((0, xK_m),      runOrRaise "mumble" (className =? "Mumble"))
               -- task manager
               , ((0, xK_Delete), runOrRaise "xfce4-taskmanager" (className =? "Xfce4-taskmanager"))
               -- deluge
               , ((0, xK_d),      runOrRaise "deluge" (className =? "Deluge"))
               ])
        ]
        

-- MOUSE SHORTCUTS

-- mouse buttons are defined at the bottom of the file
setMouseBindings (XConfig {XMonad.modMask = mod}) = M.fromList $

        -- mod-l_button, set floating mode and move by dragging
        [ ((mod              , mLeft),    (\w -> focus w >> mouseMoveWindow w
	                                                     >> windows W.shiftMaster))
        -- mod-r_button, resize window by dragging
        , ((mod              , mRight),   (\w -> focus w >> mouseResizeWindow w
                                                         >> windows W.shiftMaster))
        -- mouse wheel to move between workspaces
        , ((mod              , mWheelDn), (\_ -> moveTo Prev NonEmptyWS))
        , ((mod              , mWheelUp), (\_ -> moveTo Next NonEmptyWS))
        -- gestures
        , ((mod              , mRight), mouseGesture setGestures)
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


-- STATUS BAR

setBar = "xmobar"
-- toggle key
setToggleKey XConfig {XMonad.modMask = mod} = (mod, xK_b)
-- PP settings
setPP = defaultPP { ppCurrent = xmobarColor (setColors !! 0) "" . map toUpper --wrap "<" ">"
                  , ppTitle   = xmobarColor (setColors !! 0) "" . shorten 40
                  , ppVisible = map toUpper -- wrap "(" ")"
                  , ppHidden  = wrap "" ""
                  , ppUrgent  = xmobarColor "red" "red"
                  }


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
              , manageHook         = manageDocks <+> setManageHook  
              , layoutHook         = setLayoutHook 
              , logHook            = setLogHook <+> logHook xfceConfig
              , workspaces         = setWorkspaces
              }


