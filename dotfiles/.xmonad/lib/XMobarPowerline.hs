module XMobarPowerline where

import Codec.Binary.UTF8.String
import Control.Monad
import Data.Default
import Data.Maybe

import XMonad

import XMonad.Hooks.UrgencyHook
import XMonad.Util.NamedWindows
import XMonad.Util.WorkspaceCompare

import qualified XMonad.StackSet as S

import Theme

type ColorString = String

wrap :: String -> String -> String -> String
wrap _ _ "" = ""
wrap l r m  = l ++ m ++ r

colored :: ColorString -> ColorString -> String -> String
colored fg bg = wrap ("<fc=" ++ fg ++ "," ++ bg ++ ":0>") "</fc>"

font :: Int -> String -> String
font i = wrap ("<fn=" ++ (show i) ++ ">") "</fn>"

box :: Int -> ColorString -> String -> String
box w c = wrap ("<box width=" ++ (show w) ++ " color=" ++ c ++ ">") "</box>"

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
textSeg fg bg s = Seg bg bg . colored fg bg $ s
--textSeg fg bg s = Seg bg bg . colored fg bg . box 2 bg $ s

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
powerlineJoin sep s1 s2 = Seg { text = text s1 ++ (format sepString) ++ text s2
                              , startColor = startColor s1
                              , endColor = endColor s2
                              }
  where
    s1End = endColor s1
    s2Start = startColor s2
    fn = fontNumber sep
    sepString = if s1End == s2Start
                then colored (sameColor sep) s1End $ sameSep sep
                --else colored s1End s2Start $ differentSep sep
                else colored s2Start s1End $ differentSep sep
    format = font (fontNumber sep)

data PowerlinePP = PowerlinePP { segCurrent          :: String -> Seg
                               , segVisible          :: String -> Seg
                               , segHidden           :: String -> Seg
                               , segHiddenNoWindows  :: String -> Seg
                               , segVisibleNoWindows :: Maybe (String -> Seg)
                               , segUrgent           :: String -> Seg
                               , sepWs            :: PowerlineSep
                               , segBeforeWorkspaces :: Seg

                               , sepTabs           :: PowerlineSep
                               , isTabbed       :: String -> Bool
                               , tabAreaWidth         :: Int
                               , segNormalView       :: String -> Seg
                               , segActiveTab        :: String -> Seg
                               , segInactiveTab      :: String -> Seg
                               , segBeforeTabs       :: Seg
                               , segAfterTabs        :: Seg

                               , segSepWsTabs              :: PowerlineSep

                               , extraLoggers           :: [X (Maybe String)]
                               , wsSort             :: X WorkspaceSort
                               , outputHandler           :: String -> IO ()
                               }

instance Default PowerlinePP where
  def = PowerlinePP { segCurrent          = textSeg base03 magenta . font 2
                    , segVisible          = textSeg base0 base03 . font 2
                    , segHidden           = textSeg base1 base02 . font 2
                    , segHiddenNoWindows  = textSeg base01 base03 . font 2
                    , segVisibleNoWindows = Just $ const $ textSeg base03 base03 ""
                    , segUrgent           = textSeg base03 red
                    , sepWs               = PowerlineSep 3 "\57534" "\57529" base04
                    , segBeforeWorkspaces = textSeg base04 base04 " "
                    , wsSort              = getSortByIndex

                    , sepTabs             = PowerlineSep 3 "\57534" "\57529" base04
                    , tabAreaWidth        = 100
                    , isTabbed            = (== "t")
                    , segNormalView       = textSeg base0 base03
                    , segActiveTab        = textSeg base03 cyan
                    , segInactiveTab      = textSeg base0 base03
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
  focusedWindow <- sequence $ fmap getName $ S.peek winset
  allWindows <- sequence $ map getName $ S.index winset
  let tabs = powerlineTabs layoutDesc focusedWindow allWindows pp

  return $ encodeString $ show $ powerlineJoin (segSepWsTabs pp) workspaces tabs

powerlinePP :: PowerlinePP -> X ()
powerlinePP pp = powerlinePPString pp >>= io . outputHandler pp

powerlineWorkspaces :: WorkspaceSort -> [Window] -> PowerlinePP -> WindowSet -> Seg
powerlineWorkspaces sort' urgents pp s =
  foldl (powerlineJoin sep) (segBeforeWorkspaces pp) . map format . sort' $
  map S.workspace (S.current s : S.visible s) ++ S.hidden s
  where
    sep      = sepWs pp
    current  = S.currentTag s
    visibles = map (S.tag . S.workspace) (S.visible s)
    format w = printer pp (S.tag w)
      where
        printer | any (\x -> maybe False (== S.tag w) (S.findTag x s)) urgents = segUrgent
                | S.tag w == current = segCurrent
                | S.tag w `elem` visibles && isJust (S.stack w) = segVisible
                | S.tag w `elem` visibles = liftM2 fromMaybe segVisible segVisibleNoWindows
                | isJust (S.stack w) = segHidden
                | otherwise = segHiddenNoWindows

powerlineTabs :: String -> Maybe NamedWindow -> [NamedWindow] -> PowerlinePP -> Seg
powerlineTabs l f ws pp = if (isTabbed pp) l
                          then powerlineJoin sep tabbedView postfix
                          else powerlineJoin sep normalView postfix
  where
    sep = sepTabs pp
    prefix = segBeforeTabs pp
    postfix = segAfterTabs pp
    normalView = powerlineJoin sep prefix . segNormalView pp . forceLength width $ name
      where
        width = tabAreaWidth pp
        name = fromMaybe "" (fmap show f)
    tabbedView = foldl (powerlineJoin sep) prefix $ map format ws
      where
        format w = if maybe False (== unName w) (fmap unName f)
                   then (segActiveTab pp) . forceLength activeLen $ show w
                   else (segInactiveTab pp) . forceLength inactiveLen $ show w
          where
            width = tabAreaWidth pp - length ws + 1
            inactiveLen = width `div` length ws
            activeLen = inactiveLen + width `rem` length ws
