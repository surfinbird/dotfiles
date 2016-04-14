--                                                           -*- haskell -*-
{-# LANGUAGE OverloadedStrings #-} -- needed for DBus constants

import XMonad
import XMonad.Prompt

import XMonad.Actions.Volume        -- for Volume controls
import XMonad.Actions.GridSelect    -- for goToSelected
import XMonad.Actions.CycleWS       -- for prevWS, nextWS, anyWS, toggleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.CopyWindow    -- copy, kill1
import XMonad.Actions.MouseGestures
import XMonad.Actions.GroupNavigation -- nextMatch, History

import XMonad.Config.Desktop        -- general desktop config
import XMonad.Config.Gnome          -- .. with Gnome spesifics
import XMonad.Config.Kde            -- .. with Kde spesifics
import XMonad.Config.Xfce           -- .. with Xfce spesifics

import XMonad.Hooks.EwmhDesktops    -- ewmh, fullscreenEventHook
import XMonad.Hooks.ManageDocks     -- avoidStruts, docksEventHook, manageDocks, ToggleStruts
import XMonad.Hooks.ManageHelpers   -- isFullscreen, doFullFloat
import XMonad.Hooks.UrgencyHook     -- withUrgencyHook, NoUrgencyHook, focusUrgent
import XMonad.Hooks.FadeInactive    -- fadeInactiveLogHook (need xcompmgr)
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.SetWMName

import XMonad.Layout.NoBorders      -- lessBorders, OnlyFloat
import XMonad.Layout.Grid           -- Grid
import XMonad.Layout.PerWorkspace   -- onWorkspace
import XMonad.Layout.IM		    -- withIM
import XMonad.Layout.Reflect        -- reflectHoriz
import XMonad.Layout.MultiToggle    -- mkToggle, Toggle
import XMonad.Layout.MultiToggle.Instances -- MIRROR, NBFULL
import XMonad.Layout.ThreeColumns

import XMonad.Util.EZConfig         -- additionalKeysP
import XMonad.Util.Loggers          --

import XMonad.Prompt
import XMonad.Prompt.XMonad

import qualified XMonad.Actions.DynamicWorkspaceOrder as DWO
import qualified XMonad.StackSet as W


import System.Posix.Env (getEnv)
import Data.Maybe (maybe, fromMaybe)
import Data.List

import qualified Data.Map as M

-- for dbus messages
import qualified DBus as D
import qualified DBus.Client as DC
import qualified Codec.Binary.UTF8.String as UTF8



myWorkspaces = [ "code", "hack", "web", "chat" ] --,  "media" ]

doMakeShift :: WorkspaceId -> ManageHook
doMakeShift s = do
  liftX $ addHiddenWorkspace s
  doShift s

-- Use 'xprop WM_CLASS WM_NAME' for useful info.
myManageHook = composeOne
  [ isFullscreen                 -?> doFullFloat
  , isDialog                     -?> doFloat
  , className =? "Firefox"       -?> doMakeShift "web"
  , className =? "google-chrome" -?> doMakeShift "web"
  , className =? "Thunderbird"   -?> doMakeShift "web"
  , className =? "Geary"         -?> doMakeShift "web"
  , className =? "Pidgin"        -?> doMakeShift "chat"
  , title     =? "irc"           -?> doMakeShift "chat"
  , className =? "Spotify"       -?> doMakeShift "media"
  , title     =? "extvideotest"  -?> doFloat
  , title     =? "MainWindow"    -?> doFloat    
  , className =? "Fldiff"        -?> doFloat
  , title     =? "Ediff"         -?> doFloat
  , title     =? "PD-100 Remote" -?> doFloat
  , title =? "Robot Framework" -?> doFloat
  ]


myLayoutHook = lessBorders Screen $
               avoidStruts $
               onWorkspace "chat" imLayout $
               mkToggle (single REFLECTX) $
               mkToggle (single REFLECTY) $
               mkToggle (single NBFULL) $
               mkToggle (single MIRROR) $
               tall ||| ThreeCol 1 (3/100) (1/2) ||| ThreeColMid 1 (3/100) (1/2) ||| Mirror tall ||| Full ||| Grid
  where
    tall = Tall 1 (3/100) (1/2) -- Windows-in-master (increment) (master-ratio)
    imLayout = mkToggle (single NBFULL) $ reflectHoriz $ withIM (1/5) (Role "buddy_list") Grid

wsPP = xmobarPP { ppOrder  = \(ws:_:f:r)   -> ws:f:r                 
                , ppTitle = xmobarColor "#698b22" ""
                , ppCurrent = xmobarColor "#8b7500" ""
                , ppUrgent = xmobarColor "#ff0000" ""}

--  colors match Ubuntu Human theme and Gnome panels
background = "'#222222'"
foreground = "'#bbbbbb'"
selectedBg = "'#005577'"
selectedFg = "'#eeeeee'"
 
-- height matches Ubuntu top Gnome panel
barHeight = "24"

--  font intended to match Ubuntu default application font
appFontXft = "xft:DejaVu Sans Mono:size=10:antialias=true"

-- GridSelect config
myGSConfig = (buildDefaultGSConfig myColorizer)
  { gs_font       = appFontXft
  , gs_cellwidth  = 400
  , gs_cellheight = 80
  , gs_navigate   = navNSearch
  }

myColorizer w a =
  if a
  then return ("#005577", "#eeeeee")
  else return ("#222222", "#bbbbbb")

myXPConfig = defaultXPConfig
  { font        = appFontXft
  , fgColor     = "#bbbbbb"
  , bgColor     = "#222222"
  , fgHLight    = "#eeeeee"
  , bgHLight    = "#005577"
  , height      = 24
  , borderColor = "#888888"
  }

-- dmenu config
myDmenuTitleBar =
    "exec `dmenu_run\
        \ -p 'Run:'\
        \ -i\
    \`"

-- \ -fn DejaVu Sans Mono:size=10:antialias=true\

-- bind it all together
main :: IO ()
main = do
  -- connect to DBus
  dbus <- DC.connectSession
  getWellKnownName dbus
  -- load suitable session config
  mSession <- getEnv "DESKTOP_SESSION"
  let sessionConfig = getSessionConfig $ fromMaybe "xmonad" mSession
  -- start xmonad
  xmonad $
    withUrgencyHook NoUrgencyHook $
    ewmh
    sessionConfig
    { modMask            = mod4Mask
    , borderWidth        = 2
    , normalBorderColor  = "#696969"
    , focusedBorderColor = "#ffa500"
    , handleEventHook    = handleEventHook sessionConfig
                           <+> fullscreenEventHook
                           <+> docksEventHook
    , manageHook         = manageHook sessionConfig
                           <+> myManageHook 
    , layoutHook         = myLayoutHook
    , workspaces         = myWorkspaces
    , logHook            = dynamicLogWithPP (dbusPP dbus)
                           -- >> historyHook
                           -- >> fadeInactiveLogHook 0.75
    , startupHook        = startupHook sessionConfig
                           <+> setWMName "LG3D" -- for JAVA borkenness
    , terminal           = "xterm"
    } `additionalKeysP` (
      [ ("M-y",                    focusUrgent)
      , ("M-g",                    goToSelected myGSConfig)
      , ("M-S-g",                  gridselectWorkspace myGSConfig (\ws -> W.greedyView ws))
      , ("M-a",                    toggleWS) -- nextMatch History (return True))
      , ("M-C-x",                  xmonadPrompt myXPConfig)
      , ("M-p",                    spawn myDmenuTitleBar)
      -- EmptyWS - WS without windows
      -- NonEmptyWS - WS with windows
      -- HiddenWS - Any WS not shown
      -- HiddenNonEmptyWS - Any WS not shown with windows
      -- AnyWS - any WS
      , ("M-<L>",                  removeEmptyWorkspaceAfterExcept myWorkspaces $
                                   DWO.moveTo Prev HiddenWS)
      , ("M-<R>",                  removeEmptyWorkspaceAfterExcept myWorkspaces $
                                   DWO.moveTo Next HiddenWS)
      , ("M-S-<R>",                DWO.shiftTo Next AnyWS >> DWO.moveTo Next AnyWS)
      , ("M-S-<L>",                DWO.shiftTo Prev AnyWS >> DWO.moveTo Prev AnyWS)
      , ("M-C-<R>",                DWO.swapWith Next AnyWS)
      , ("M-C-<L>",                DWO.swapWith Prev AnyWS)
      , ("M-n",                    addWorkspacePrompt myXPConfig)
      , ("M-m",                    withWorkspace myXPConfig (windows . W.shift))
      , ("M-c",                    withWorkspace myXPConfig (windows . copy))
      , ("M-S-c",                  kill1)
      , ("M-s",                    windows copyToAll) -- @@ Make focused window always visible
      , ("M-S-s",                  killAllOtherCopies) -- @@ Toggle window state back
      , ("M-x",                    sendMessage $ Toggle MIRROR)
      , ("M-f",                    sendMessage $ Toggle NBFULL)
      , ("M-S-x",                  sendMessage $ Toggle REFLECTX)
      , ("M-S-y",                  sendMessage $ Toggle REFLECTY)
        -- Sound bindings
      , ("<XF86AudioLowerVolume>", lowerVolume 3 >> return ())
      , ("<XF86AudioRaiseVolume>", raiseVolume 3 >> return ())
      , ("<XF86AudioMute>",        toggleMute    >> return ()) -- toggleMute
      , ("M-C-l",                  spawn "gnome-screensaver-command --lock")
      ]
      ++
      [ ("M-" ++ [key], action)
      | (key, action) <- zip "123456789"
                         (map (removeEmptyWorkspaceAfterExcept myWorkspaces .
                               withNthWS W.greedyView
                              ) [0..])
      ]
      {-
      ++
      -- possibly fixed so you can show the same screen on more than one screen
      [ (otherModMasks ++ "M-" ++ [key], action tag)
      | (tag, key)  <- zip myWorkspaces "123456789"
      , (otherModMasks, action) <- [ ("", windows . W.view) -- was W.greedyView
                                   , ("S-", windows . W.shift)]
      ]
      -}
      ) `additionalMouseBindings`
    [ ((mod4Mask .|. shiftMask, button3), mouseGesture myGestures)
    ]

myGestures = M.fromList
           [ ([], focus)
           , ([U], \w -> focus w >> windows W.swapUp)
           , ([D], \w -> focus w >> windows W.swapDown)
           , ([R, D], \_ -> sendMessage NextLayout)
           ]



-- Since withNthWorkspace uses the wrong sort
withNthWS :: (String -> WindowSet -> WindowSet) -> Int -> X ()
withNthWS job wnum = do sort <- DWO.getSortByOrder
                        ws <- gets (map W.tag . sort . W.workspaces . windowset)
                        case drop wnum ws of
                          (w:_) -> windows $ job w
                          [] -> return ()

-- Session selectors
getSessionConfig "gnome"        = gnomeConfig
getSessionConfig "kde"          = kde4Config
getSessionConfig "xfce"         = xfceConfig
getSessionConfig "xmonad-gnome" = gnomeConfig
getSessionConfig "gnome-xmonad" = gnomeConfig
getSessionConfig _              = desktopConfig -- Adds M-b = ToggleStruts and avoidStruts

-- Map some layout names to nicer ones
layoutNameMap x = case x of
--  "Tall" -> "│├─"
  "Mirror Tall" -> "Wide"
  "ReflectX IM Grid" -> "IM"
  _ -> x

-- http://xmonad.org/xmonad-docs/
-- xmonad-contrib/src/XMonad-Hooks-DynamicLog.html#defaultPP
dbusPP :: DC.Client -> PP
dbusPP dbus = defaultPP
    { ppOutput          = dbusOutput dbus -- putStrLn
    , ppCurrent         = pangoColor "lightgreen" . wrap "[" "]" . pangoSanitize
                          -- wrap "[" "]"
    , ppVisible         = pangoColor "yellow" . wrap "(" ")" . pangoSanitize
                          -- wrap "<" ">"
    , ppHidden          = pangoSanitize -- id
    , ppHiddenNoWindows = pangoColor "darkgray" . pangoSanitize -- const ""
    , ppUrgent          = pangoColor "red" . pangoSanitize -- id
    , ppSep             = pangoColor "orange" " ••• " -- " : "
    , ppWsSep           = " " -- " "
    , ppTitle           = pangoSanitize -- shorten 80
    , ppLayout          = layoutNameMap -- id
--    , ppOrder           = \(ws:l:t:r) -> (l:ws:t:r) -- id
    , ppSort            = DWO.getSortByOrder -- getSortByIndex
--    , ppExtras = [ loadAvg, aumixVolume, battery, date "%FT%T" ] -- []
    }

-- DynamicLog to DBus

getWellKnownName :: DC.Client -> IO ()
getWellKnownName dbus = do
  DC.requestName dbus "org.xmonad.Log"
                        [DC.nameAllowReplacement, DC.nameReplaceExisting, DC.nameDoNotQueue]
  return ()

dbusOutput :: DC.Client -> String -> IO ()
dbusOutput dbus str = do
  let signal = (D.signal "/org/xmonad/Log" "org.xmonad.Log" "Update") {
        D.signalBody = [D.toVariant $ UTF8.decodeString str]
        }
  DC.emit dbus signal

pangoColor :: String -> String -> String
pangoColor fg = wrap left right
  where
    left  = "<span foreground=\"" ++ fg ++ "\">"
    right = "</span>"

pangoSanitize :: String -> String
pangoSanitize = foldr sanitize ""
  where
    sanitize '>'  xs = "&gt;" ++ xs
    sanitize '<'  xs = "&lt;" ++ xs
    sanitize '\"' xs = "&quot;" ++ xs
    sanitize '&'  xs = "&amp;" ++ xs
    sanitize x    xs = x:xs
