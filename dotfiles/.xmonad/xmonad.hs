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

import XMonad.Config.Desktop

import XMonad.Layout.Grid
import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts
import XMonad.Layout.ThreeColumns

import XMonad.ManageHook

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import qualified Data.Map        as M
import qualified XMonad.StackSet as S

import Theme
-- import XMobarPowerline
import FancyXMobar
import WithSlaves
import BlurWallpaper
import XMonad.Layout.Cantor
import XMonad.Layout.ZoomSecondary
import XMonad.Action.SwapSlaves

main = do h <- spawnPipe "xmobar"
          xmonad $ docks $ ewmh def
                     { terminal           = theTerminal
                     , modMask            = mod4Mask
                     , focusFollowsMouse  = True
                     , workspaces         = theWorkspaces

                     , borderWidth        = 3
                     , focusedBorderColor = yellow
                     , normalBorderColor  = base02

                     , manageHook         = theManageHook
                     , layoutHook         = theLayoutHook
                     , logHook            = theLogHook h
                     , handleEventHook    = theHandleEventHook
                     , startupHook        = theStartupHook

                     , mouseBindings      = theMouse
                     , keys               = theWindowMovementKeys
                     }
                     `additionalKeysP` theKeys

theTerminal = "st"

theWorkspaces = clickable [" 일 "," 이 "," 삼 "," 사 "," 오 "," 육 "," 칠 "," 팔 "," 구 "]
--theWorkspaces = clickable [" 1 "," 2 "," 3 "," 4 "," 5 "," 6 "," 7 "," 8 "," 9 "]
                ++ ["NSP"]
    where
      clickable ws = map action $ zip ws [1..]
      action (w, i) = wrap ("<action=`xdotool key super+" ++ show i ++ "`>") "</action>" w

theManageHook = composeAll
    -- never open as master
    [ className =? "URxvt"         --> doF (S.swapDown)
    , className =? "st-256color"   --> doF (S.swapDown)
    , className =? "Thunar"        --> doF (S.swapDown)
    , className =? "Emacs"         --> doF (S.swapDown)
    -- default workspaces
    -- don't focus xfce4-notifyd
    , className =? "Xfce4-notifyd" --> doIgnore
    , title     =? "Whisker Menu"  --> doFloat
    ]
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads

scratchpads = [ NS "htop" "st -A 0.9 -C -e htop" (title =? "htop")
                nonFloating
              , NS "taskmgr" "xfce4-taskmanager" (className =? "Xfce4-taskmanager")
                nonFloating
              , NS "scratch" "st -A 0.9 -C -c scratch" (className =? "scratch") $
                customFloating (S.RationalRect 0.0 0.70 0.50 0.30)
              , NS "julia" "st -A 0.9 -C -c julia -e julia" (className =? "julia") $
                customFloating (S.RationalRect 0.0 0.10 0.50 0.30)
              , NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol") $
                customFloating (S.RationalRect 0.69 0.10 0.30 0.40)
              ]

theLogHook h = blurWallpaper wallpapers
               <+> (ewmhDesktopsLogHook >> updatePointer (0.5, 0.5) (0, 0))
               <+> (fancyPP $ thePP h)

theLayoutHook = avoidStruts $ toggleLayouts tabbed cantors
  where
    hints'   = layoutHintsWithPlacement (0.5, 0.0)
    spacing' = spacingRaw False (Border 5 10 10 10) True (Border 10 10 10 10) True
    tall     = TallCantor (3/100) (1/2) (3/100) (1/2)
    mirror   = Cantor (3/100) (1/2)
    zoom     = zoomSecondary (9/10) False
    cantors  = hints' . spacing' . zoom $ (tall ||| mirror)
    tabbed   = named "t" $ noBorders Full
    full     = named "f" $ noBorders Full

theHandleEventHook = hintsEventHook

theStartupHook = setWMName "LG3D"

--------------
-- Keyboard --
--------------
deprecated :: String -> X ()
deprecated k = spawn $ "notify-send -i " ++ icon ++ " \"Use " ++ k ++ ".\""
  where
    icon = "\"/usr/share/icons/Papirus/128x128/apps/abrt.svg\""

-- Execute `action` if current layout name == `name`, otherwise execute `default`.
onLayout :: String -> X () -> X () -> X ()
onLayout name action def = do
  workspaces <- gets windowset
  let layoutDesc = description . S.layout . S.workspace . S.current $ workspaces
  if layoutDesc == name
  then action
  else def

rotUp' :: X ()
rotUp' = onLayout "t" (windows S.swapUp) (windows swapSlavesUp)

rotDown' :: X ()
rotDown' = onLayout "t" (windows S.swapDown) (windows swapSlavesDown)

theKeys =
    -- focus and window rotation
    [ ("M-x",               windows S.focusDown)
    , ("M1-<Tab>",          windows S.focusDown)
    , ("M1-S-<Tab>",        windows S.focusUp)
    , ("M-y",               windows S.focusUp)
    , ("M-z",               windows S.focusUp)
    , ("M-S-x",             rotDown')
    , ("M-S-y",             rotUp')
    , ("M-S-z",             rotUp')
    , ("M-s",               dwmpromote)
    , ("M-C-s",             dwmPromoteSlaves)
    , ("M-S-s",             windows S.focusMaster)
    -- window sizing and gaps
    , ("M-C-x",             sendMessage Expand)
    , ("M-C-y",             sendMessage Shrink)
    , ("M-C-z",             sendMessage Shrink)
    , ("M-M1-x",            sendMessage ExpandSecondary)
    , ("M-M1-y",            sendMessage ShrinkSecondary)
    , ("M-M1-z",            sendMessage ShrinkSecondary)
    , ("M-C-l",             sendMessage Expand)
    , ("M-C-h",             sendMessage Shrink)
    , ("M-C-j",             sendMessage ExpandSecondary)
    , ("M-C-k",             sendMessage ShrinkSecondary)
    --, ("M-C-v",             decScreenWindowSpacing 1)
    --, ("M-C-c",             incScreenWindowSpacing 1)
    -- misc
    , ("M-b",               sendMessage ToggleStruts)
    , ("M-S-t",             withFocused $ windows . S.sink)
    -- workspaces
    , ("M-<Tab>",           toggleWS' ["NSP"])
    , ("M-d",               moveTo Next EmptyWS)
    , ("M-S-d",             shiftTo Next EmptyWS)
    , ("M-w",               nextScreen)
    , ("M-S-w",             shiftNextScreen)
    , ("M-C-w",             swapNextScreen)
    , ("M-c",               moveTo Prev NonEmptyWS)
    , ("M-v",               moveTo Next NonEmptyWS)
    , ("M-S-c",             shiftToPrev)
    , ("M-S-v",             shiftToNext)
    -- killing
    , ("M-q q",             kill1)
    , ("M-q M-q",           kill)
    , ("M-q a",             killAll)
    , ("M-q s",             killSlaves)
    -- layouts
    , ("M-<Space>",         sendMessage $ Toggle "t")
    , ("M-C-<Space>",       sendMessage ZoomToggle)
    , ("M-<F1>",            sendMessage FirstLayout)
    , ("M-<F2>",            sendMessage FirstLayout >> sendMessage NextLayout)
    -- application shortcuts and scratchpads
    , ("M-S-p",             spawn "screenshot")
    , ("M-`",               namedScratchpadAction scratchpads "scratch")
    , ("M-C-`",             namedScratchpadAction scratchpads "julia")
    , ("M-i",               spawn "notify-window-title")
    , ("M-C-i",             spawn "copy-window-title")
    , ("M-<Delete>",        namedScratchpadAction scratchpads "htop")
    , ("M-S-<Delete>",      namedScratchpadAction scratchpads "taskmgr")
    , ("M-r",               spawn "dmenu-launch")
    , ("M-e",               deprecated "M-f")
    , ("M-t",               spawn theTerminal)
    , ("M-M1-t",            spawn "terminal-at-window-title")
    , ("M-p",               namedScratchpadAction scratchpads "pavucontrol")
    , ("M-m",               spawn "emacsclient -c")
    , ("M-.",               spawn "dmenu-latexsub")
    , ("M-f",               spawn "dmenu-open")
    -- actions
    , ("M-g",               spawn "dmenu-find-window")
    -- audio
    , ("M-=",               spawn "pulsemixer --change-volume +5")
    , ("M--",               spawn "pulsemixer --change-volume -5")
    , ("M-S-=",             spawn "pulsemixer --change-volume +1")
    , ("M-S--",             spawn "pulsemixer --change-volume -1")
    , ("M-C-=",             spawn "pulsemixer --set-volume 100")
    , ("M-C--",             spawn "pulsemixer --toggle-mute")
    , ("<XF86AudioMute>",   spawn "pulsemixer --toggle-mute")
    ]

theWindowMovementKeys c@(XConfig {XMonad.modMask = mod}) =
    M.fromList $
         -- move windows between workspaces
         [ ((mod .|. controlMask, k), windows $ swapWithCurrent i)
               | (i, k) <- zip theWorkspaces [xK_1..]
         ] ++
         -- copy windows between workspaces
         [ ((m .|. mod, k), windows $ f i) | (i, k) <- zip theWorkspaces [xK_1 ..]
         , (f, m) <- [ (S.view, 0)
                     , (\w -> S.greedyView w . S.shift w, shiftMask)
                     , (copy, shiftMask .|. controlMask)]
         ]

theMouse (XConfig {XMonad.modMask = mod}) =
    M.fromList $
         [ ( (mod, 1 :: Button)
           , (\w -> focus w >> mouseMoveWindow w >> windows S.shiftMaster)
           )
         , ( (mod .|. controlMask, 1 :: Button)
           , (\w -> focus w >> mouseResizeWindow w >> windows S.shiftMaster)
           )
         ]
