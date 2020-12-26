module FancyXMobar where

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

colored :: String -> String -> String -> String
colored fg bg = wrap ("<fc=" ++ fg ++ "," ++ bg ++ ":0>") "</fc>"

font :: Int -> String -> String
font i = wrap ("<fn=" ++ show i ++ ">") "</fn>"

underline :: Int -> String -> String -> String
underline p c = wrap ("<box type=Bottom width=" ++ show p
                      ++ " color=" ++ c ++ ">") "</box>"

borders :: Int -> String -> String -> String
borders p c = wrap ("<box type=HBoth width=" ++ show p
                    ++ " color=" ++ c ++ ">") "</box>"

action :: String -> String -> String
action a = wrap ("<action=`" ++ a ++ "`>") "</action>"

forceLength :: Int -> String -> String
forceLength n s | length s < n = take n (s ++ repeat ' ')
                | otherwise    = take (n - 2) s ++ "… "

-- theme

data FancyString = FancyString { text    :: String
                               , leftBg  :: String
                               , rightBg :: String
                               }
instance Show FancyString where
  show = text

fancy :: String -> String -> String -> FancyString
fancy fg bg s = FancyString (colored fg bg s) bg bg

data FancyPP = FancyPP { backgroundColor    :: String
                       , sameSeparator      :: String
                       , diffSeparator      :: String
                       , separatorFont      :: Int

                       , wsPrefix           :: FancyString
                       , tabWSSeparator     :: FancyString
                       , tabsPostfix        :: FancyString

                       , currentWSTheme     :: String -> FancyString
                       , visibleWSTheme     :: String -> FancyString
                       , hiddenWSTheme      :: String -> FancyString
                       , hiddenEmptyWSTheme :: String -> FancyString
                       , urgentWSTheme      :: String -> FancyString

                       , isTabbedLayout     :: String -> Bool
                       , tabAreaWidth       :: Int
                       , noWindowsTheme     :: String -> FancyString
                       , untabbedTheme      :: String -> FancyString
                       , activeTabTheme     :: String -> FancyString
                       , inactiveTabTheme   :: String -> FancyString

                       , extraLoggers       :: [X (Maybe String)]
                       , wsSort             :: X WorkspaceSort
                       , outputHandler      :: String -> IO ()
                       }

glue :: FancyPP -> FancyString -> FancyString -> FancyString
glue pp (FancyString t l r) (FancyString t' l' r')
  | r /= l' = FancyString resDiff l r'
  | r == l' = FancyString resSame l r
  where
    resDiff = t ++ (font' . colored l' r) (diffSeparator pp) ++ t'
    resSame = t ++ (font' . colored (backgroundColor pp) l') (sameSeparator pp) ++ t'
    font' = font (separatorFont pp)

fancyPPString :: FancyPP -> X String
fancyPPString pp = do
  winset <- gets windowset

  -- workspaces
  urgents <- readUrgents
  sort' <- wsSort pp
  let workspaces = fancyWorkspaces sort' urgents pp winset

  -- tabs
  let layoutDesc = description . S.layout . S.workspace . S.current $ winset
  focusedWindow <- sequence $ getName <$> S.peek winset
  allWindows <- mapM getName $ S.index winset
  let tabs = fancyTabs layoutDesc focusedWindow allWindows pp

  return . encodeString . show $ glue pp workspaces tabs

fancyPP :: FancyPP -> X ()
fancyPP pp = fancyPPString pp >>= io . outputHandler pp

fancyWorkspaces :: WorkspaceSort -> [Window] -> FancyPP -> WindowSet -> FancyString
fancyWorkspaces sort' urgents pp s =
  foldl (glue pp) (wsPrefix pp) . map format . sort' . filter notNSP $ workspaces
  where
    workspaces = map S.workspace (S.current s : S.visible s) ++ S.hidden s
    notNSP     = (/= "NSP") . S.tag
    current    = S.currentTag s
    visibles   = map (S.tag . S.workspace) (S.visible s)
    format w   = printer pp (S.tag w)
      where
        printer | any (\x -> Just (S.tag w) == S.findTag x s) urgents = urgentWSTheme
                | S.tag w == current = currentWSTheme
            --- | S.tag w `elem` visibles && isJust (S.stack w) = visibleWSTheme
                | S.tag w `elem` visibles = visibleWSTheme
                | isJust (S.stack w) = hiddenWSTheme
                | otherwise = hiddenEmptyWSTheme

fancyTabs :: String -> Maybe NamedWindow -> [NamedWindow] -> FancyPP -> FancyString
fancyTabs l f ws pp = glue pp prefix $ glue pp view postfix
  where
    prefix  = tabWSSeparator pp
    postfix = tabsPostfix pp
    view    = if isTabbedLayout pp l
              then glue pp (tabbedView pp f ws) postfix
              else glue pp (normalView pp f) postfix

noWindows :: FancyPP -> FancyString
noWindows pp = noWindowsTheme pp . forceLength (tabAreaWidth pp) $ ""

normalView :: FancyPP -> Maybe NamedWindow -> FancyString
normalView pp = maybe (noWindows pp) (format . (' ':) . show)
    where
      format = untabbedTheme pp . xmobarRaw . forceLength (tabAreaWidth pp)

tabbedView :: FancyPP -> Maybe NamedWindow -> [NamedWindow] -> FancyString
tabbedView pp _ [] = noWindows pp
tabbedView pp f (w:ws) = foldl (glue pp) w' ws'
    where
      w'       = format w
      ws'      = map format ws
      width    = tabAreaWidth pp - length ws
      l        = width `div` (length ws + 1)
      l'       = l + width `rem` (length ws + 1)
      click w  = action $ "xdotool windowactivate " ++ show (unName w)
      format w =
        if maybe False ((== unName w) . unName) f
        then activeTabTheme pp . click w . xmobarRaw . forceLength l' $ ' ':show w
        else inactiveTabTheme pp . click w . xmobarRaw . forceLength l $ ' ':show w
