module XMobarPowerline where

import Codec.Binary.UTF8.String
import Control.Monad
import Data.Default
import Data.Maybe

import XMonad

import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare

import XMonad.Hooks.DynamicLog
import qualified XMonad.StackSet as S

import Theme

type ColorString = String

colored :: ColorString -> ColorString -> String -> String
colored fg bg = wrap ("<fc=" ++ fg ++ "," ++ bg ++ ":0>") "</fc>"

font :: Int -> String -> String
font i = wrap ("<fn=" ++ show i ++ ">") "</fn>"

box :: Int -> ColorString -> String -> String
box w c = wrap ("<box width=" ++ show w ++ " color=" ++ c ++ ">") "</box>"

underline :: ColorString -> String -> String
underline color s = "<box type=Bottom width=4 color=" ++ color ++ ">" ++ s ++ "</box>"

action :: String -> String -> String
action a = wrap ("<action=`" ++ a ++ "`>") "</action>"

forceLength :: Int -> String -> String
forceLength n s | length s < n = take n (s ++ repeat ' ')
                | otherwise    = take (n - 3) s ++ "..."

--
--
--
data Seg = Seg { startColor :: ColorString
               , endColor :: ColorString
               , text :: String
               }

instance Show Seg where
  show = text

textSeg :: ColorString -> ColorString -> String -> Seg
textSeg fg bg = Seg bg bg . colored fg bg

--
--
--
data PowerlineSep = PowerlineSep { fontNumber   :: Int
                                 , differentSep :: String
                                 , sameSep      :: String
                                 , sameColor    :: ColorString
                                 }
  deriving (Eq, Show)

powerlineJoin :: PowerlineSep -> Seg -> Seg -> Seg
powerlineJoin sep s1 s2 = Seg  (startColor s1) (endColor s2) text'
  where
    s1End = endColor s1
    s2Start = startColor s2
    text' = text s1 ++ format sepString ++ text s2
    sepString = if s1End == s2Start
                then colored (sameColor sep) s1End $ sameSep sep
                else colored s2Start s1End $ differentSep sep
    format = font (fontNumber sep)

data PowerlinePP = PowerlinePP { segCurrent          :: String -> Seg
                               , segVisible          :: String -> Seg
                               , segHidden           :: String -> Seg
                               , segHiddenNoWindows  :: String -> Seg
                               , segVisibleNoWindows :: Maybe (String -> Seg)
                               , segUrgent           :: String -> Seg
                               , sepWs               :: PowerlineSep
                               , segBeforeWorkspaces :: Seg

                               , sepTabs             :: PowerlineSep
                               , isTabbed            :: String -> Bool
                               , tabAreaWidth        :: Int
                               , segNoWindows        :: String -> Seg
                               , segNormalView       :: String -> Seg
                               , segActiveTab        :: String -> Seg
                               , segInactiveTab      :: String -> Seg
                               , segBeforeTabs       :: Seg
                               , segAfterTabs        :: Seg

                               , segSepWsTabs        :: PowerlineSep

                               , extraLoggers        :: [X (Maybe String)]
                               , wsSort              :: X WorkspaceSort
                               , outputHandler       :: String -> IO ()
                               }

instance Default PowerlinePP where
  def = PowerlinePP { --segCurrent          = textSeg base03 red . font 2
                      segCurrent          = textSeg base01 base2 . font 2 . underline red
                    , segVisible          = textSeg magenta base02 . font 2
                    , segHidden           = textSeg base1 base02 . font 2
                    , segHiddenNoWindows  = textSeg base01 base04 . font 2
                    , segVisibleNoWindows = Just $ const $ textSeg magenta base03 ""
                    , segUrgent           = textSeg base03 red
                    , sepWs               = PowerlineSep 3 "▐" "│" base04
                    , segBeforeWorkspaces = textSeg base04 base04 " "
                    , wsSort              = getSortByIndex

                    , sepTabs             = PowerlineSep 3 "▐" "│" base04
                    , tabAreaWidth        = 92
                    , isTabbed            = (== "t")
                    , segNoWindows        = textSeg base04 base04
                    , segNormalView       = textSeg base1 base03
                    , segActiveTab        = textSeg base04 base01 . underline magenta
                    , segInactiveTab      = textSeg base01 base04
                    , segBeforeTabs       = textSeg base04 base04 " "
                    , segAfterTabs        = textSeg base04 base04 " "

                    , segSepWsTabs        = PowerlineSep 3 "\57534" "\57529" base04

                    , extraLoggers        = []
                    , outputHandler       = putStrLn
                    }

powerlinePPString :: PowerlinePP -> X String
powerlinePPString pp = do
  winset <- gets windowset

  -- workspaces
  urgents <- readUrgents
  sort' <- wsSort pp
  let workspaces = powerlineWorkspaces sort' urgents pp winset

  -- tabs
  let layoutDesc = description . S.layout . S.workspace . S.current $ winset
  focusedWindow <- sequence $ getName <$> S.peek winset
  allWindows <- mapM getName $ S.index winset
  let tabs = powerlineTabs layoutDesc focusedWindow allWindows pp

  return . encodeString . show $ powerlineJoin (segSepWsTabs pp) workspaces tabs

powerlinePP :: PowerlinePP -> X ()
powerlinePP pp = powerlinePPString pp >>= io . outputHandler pp

powerlineWorkspaces :: WorkspaceSort -> [Window] -> PowerlinePP -> WindowSet -> Seg
powerlineWorkspaces sort' urgents pp s =
  foldl joinSep (segBeforeWorkspaces pp) . map format . sort' . filter notNSP $ workspaces
  where
    workspaces = map S.workspace (S.current s : S.visible s) ++ S.hidden s
    notNSP     = (/= "NSP") . S.tag
    joinSep    = powerlineJoin $ sepWs pp
    current    = S.currentTag s
    visibles   = map (S.tag . S.workspace) (S.visible s)
    format w   = printer pp (S.tag w)
      where
        printer | any (\x -> Just (S.tag w) == S.findTag x s) urgents = segUrgent
                | S.tag w == current = segCurrent
                | S.tag w `elem` visibles && isJust (S.stack w) = segVisible
                | S.tag w `elem` visibles = liftM2 fromMaybe segVisible segVisibleNoWindows
                | isJust (S.stack w) = segHidden
                | otherwise = segHiddenNoWindows

powerlineTabs :: String -> Maybe NamedWindow -> [NamedWindow] -> PowerlinePP -> Seg
powerlineTabs l f ws pp = joinSep prefix $ joinSep view postfix
  where
    joinSep = powerlineJoin $ sepTabs pp
    postfix = segAfterTabs pp
    prefix  = segAfterTabs pp
    view    = if isTabbed pp l
              then joinSep (tabbedView pp f ws) postfix
              else joinSep (normalView pp f) postfix

noWindows :: PowerlinePP -> Seg
noWindows pp = segNoWindows pp . forceLength (tabAreaWidth pp) $ ""

normalView :: PowerlinePP -> Maybe NamedWindow -> Seg
normalView pp = maybe (noWindows pp) (format . (' ':) . show)
    where
      format = segNormalView pp . xmobarRaw . forceLength (tabAreaWidth pp)

tabbedView :: PowerlinePP -> Maybe NamedWindow -> [NamedWindow] -> Seg
tabbedView pp _ [] = noWindows pp
tabbedView pp f (w:ws) = foldl joinSep w' ws'
    where
      joinSep  = powerlineJoin $ sepTabs pp
      w'       = format w
      ws'      = map format ws
      width    = tabAreaWidth pp - length ws
      l        = width `div` (length ws + 1)
      l'       = l + width `rem` (length ws + 1)
      click w  = wrap ("<action=`xdotool windowactivate " ++ show (unName w) ++ "`>") "</action>"
      format w = if maybe False ((== unName w) . unName) f
                 then segActiveTab pp . click w . xmobarRaw . forceLength l' $ ' ':show w
                 else segInactiveTab pp . click w . xmobarRaw . forceLength l $ ' ':show w
