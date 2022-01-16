import XMonad

import Data.Map (fromList)
import GHC.IO.Handle.Types (Handle)

import XMonad.Actions.CopyWindow
import XMonad.Actions.CycleWS
import XMonad.Actions.DwmPromote
import XMonad.Actions.SwapWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Actions.WithAll

import XMonad.Layout.LayoutHints
import XMonad.Layout.Named
import XMonad.Layout.NoBorders
import XMonad.Layout.PerWorkspace
import XMonad.Layout.Spacing
import XMonad.Layout.ToggleLayouts

import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName

import XMonad.Util.EZConfig
import XMonad.Util.Run
import XMonad.Util.NamedScratchpad

import qualified XMonad.StackSet as S

import Theme
import FancyXMobar
import WithSlaves
import BlurWallpaper
import XMonad.Layout.Cantor
import XMonad.Layout.ZoomSecondary
import XMonad.Action.SwapSlaves

main :: IO ()
main = do
  h <- spawnPipe "xmobar"
  xmonad . docks $ ewmh def { terminal           = theTerminal
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

theTerminal :: String
theTerminal = "st"

theWorkspaces :: [String]
--theWorkspaces = clickable [" 일 "," 이 "," 삼 "," 사 "," 오 "," 육 "," 칠 "," 팔 "," 구 "]
--                ++ ["NSP"]
theWorkspaces = clickable [" ⅰ "," ⅱ "," ⅲ "," ⅳ "," ⅴ "," ⅵ "," ⅶ "," ⅷ "," ⅸ "]
                ++ ["NSP"]
    where
      clickable ws = zipWith action ws [1..]
      action w i = wrap ("<action=`xdotool key super+" ++ show i ++ "`>") "</action>" w

theManageHook :: ManageHook
theManageHook = composeAll
    -- never open as master
    [ className =? "URxvt"       --> doF S.swapDown
    , className =? "st-256color" --> doF S.swapDown
    , className =? "Thunar"      --> doF S.swapDown
    , className =? "Emacs"       --> doF S.swapDown
    -- default workspaces
    -- don't focus xfce4-notifyd
    , className =? "Xfce4-notifyd" --> doIgnore
    -- zrythm autosave
    , title =? "Project Progress" --> doIgnore
    ]
    <+> manageDocks
    <+> namedScratchpadManageHook scratchpads

scratchpads :: [NamedScratchpad]
scratchpads = [ NS "htop" "st -A 0.95 -C -e htop" (title =? "htop") $
                customFloating (S.RationalRect 0.16 0.1 0.68 0.78)
              , NS "taskmgr" "xfce4-taskmanager" (className =? "Xfce4-taskmanager") $
                customFloating (S.RationalRect 0.16 0.1 0.68 0.78)
              , NS "scratch" "st -A 0.95 -C -c scratch" (className =? "scratch") $
                customFloating (S.RationalRect 0.0 0.70 0.40 0.30)
              , NS "julia" "st -A 0.95 -C -c julia -e julia" (className =? "julia") $
                customFloating (S.RationalRect 0.60 0.70 0.40 0.30)
              , NS "pavucontrol" "pavucontrol" (className =? "Pavucontrol") $
                customFloating (S.RationalRect 0.25 0.25 0.50 0.50)
              , NS "weather" "st -A 0.95 -C -c weather -e links wttr.in"
                (className =? "weather") $
                customFloating (S.RationalRect 0.16 0.1 0.68 0.78)
              , NS "calendar" "gsimplecal" (className =? "Gsimplecal") $
                customFloating (S.RationalRect 0.77 0.03 0.2 0.15)
              ]

theLogHook :: Handle -> X ()
theLogHook h = blurWallpaper wallpapers
               <+> (ewmhDesktopsLogHook >> updatePointer (0.5, 0.5) (0, 0))
               <+> fancyPP (thePP h)

theLayoutHook = avoidStruts $ toggleLayouts tabbed cantors
  where
    hints'   = layoutHintsWithPlacement (0.5, 0.0)
    spacing' = spacingRaw False (Border 5 10 10 10) True (Border 10 10 10 10) True
    tall     = TallCantor (3/100) (1/2) (3/100) (1/2)
    mirror   = Cantor (3/100) (1/2)
    zoom     = zoomSecondary 0.95 False
    cantors  = hints' . spacing' . zoom $ (tall ||| mirror)
    tabbed   = named "t" $ noBorders Full
    full     = named "f" $ noBorders Full

theHandleEventHook = hintsEventHook

theStartupHook = setWMName "LG3D" >>
  (spawn $ "xwallpaper --center " ++ sharpWallpaper) >>
  spawn "picom -b --config .config/picom.conf"

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
    , ("M-C-d",             moveTo Prev EmptyWS)
    , ("M-S-d",             shiftTo Next EmptyWS)
    , ("M-S-C-d",           shiftTo Prev EmptyWS)
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
    , ("M-C-S-w",           namedScratchpadAction scratchpads "weather")
    , ("M-C-S-c",           namedScratchpadAction scratchpads "calendar")

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


    , ("<XF86AudioLowerVolume>", spawn "pulsemixer --change-volume -5")
    , ("<XF86AudioRaiseVolume>", spawn "pulsemixer --change-volume +5")
    , ("<XF86AudioMute>",        spawn "pulsemixer --toggle-mute")
    ]

theWindowMovementKeys XConfig {XMonad.modMask = mod} =
    fromList $
         -- move windows between workspaces
         [ ((mod .|. controlMask, k), windows $ swapWithCurrent i)
               | (i, k) <- zip theWorkspaces [xK_1..]
         ] ++
         -- copy windows between workspaces
         [ ((m .|. mod, k), windows $ f i) | (i, k) <- zip theWorkspaces [xK_1..]
         , (f, m) <- [ (S.greedyView, 0)
                     , (\w -> S.greedyView w . S.shift w, shiftMask)
                     , (copy, shiftMask .|. controlMask)
                     ]
         ]

theMouse XConfig {XMonad.modMask = mod} =
    fromList [ ((mod, 1 :: Button), \w -> mouseMoveWindow w >> master)
             , ((mod .|. controlMask, 1 :: Button), \w -> mouseResizeWindow w >> master)
             ]
  where
    master = windows S.shiftMaster
