import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Layout.ResizableTile
import XMonad.Layout.Spacing
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.Tabbed
import XMonad.Layout.Named
import XMonad.Actions.GroupNavigation
import XMonad.Layout.NoBorders
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.FloatNext
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops(fullscreenEventHook,ewmh)
import XMonad.Actions.CopyWindow
import XMonad.Actions.Submap
import XMonad.Util.EZConfig (mkKeymap)
import XMonad.Util.Run
import System.IO
import Data.Maybe
import Data.List

import XMonad.Hooks.SetWMName

bg      = "#002B36"
fg      = "#93A1A1"
yellow  = "#B58900"
orange  = "#CB4B16"
red     = "#DC322F"
magenta = "#D33682"
blue    = "#268BD2"
cyan    = "#2AA198"
violet  = "#6C71C4"
green   = "#859900"

main = do
  xmonad $ myConfig

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = ewmh defaultConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , focusedBorderColor = "#bdae93"
    , normalBorderColor  = "#504945"
    , keys               = emacsStyleKeys
    , logHook            = myLogHook --statusHandle
    , layoutHook         = myLayouts
    , manageHook         = myManageHook <+> manageDocks
    , workspaces         = myWorkspaces
    , handleEventHook    = handleEventHook defaultConfig <+> fullscreenEventHook
    , startupHook        = setWMName "LG3D"
    }

-- TODO: organize this better
writeFileLn f s = writeFile f $ "x;" ++ s ++ "\n"

status_pipe = "/tmp/statuspipe.fifo"

myLogHook = dynamicLogWithPP $ def { ppOutput = writeFileLn status_pipe}

myTabsTheme = def
  { fontName            = "xft:Iosevka Lig:medium:size=10.5"
  , activeColor         = "#282828"
  , activeTextColor     = "#ebdbb2"
  , inactiveColor       = "#1d2021"
  , inactiveTextColor   = "#a89984"
  , activeBorderColor   = "#282828"
  , inactiveBorderColor = "#282828"
  , decoHeight          = 30
  }

myWorkspaces = [ "1"
               , "2"
               , "3"
               , "4"
               , "5"
               , "6"
               , "7"
               , "8"
               , "9"
               ]

myTerminal    = "alacritty"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1

myLayouts = avoidStruts
          $ (smartBorders
          $ rT
          ||| Mirror rT
          ||| Full
          ||| emptyBSP
          ||| (tabbedBottom shrinkText myTabsTheme))
  where
     rT = ResizableTall 1 (6/100) (8/13) []
     -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
     -- The default number of windows in the master pane
     nmaster = 1
     -- Default proportion of screen occupied by master pane
     ratio   = 1/2
     -- Percent of screen to increment by when resizing panes
     delta   = 3/100

showMap :: XConfig Layout -> [(String, String, X ())] -> X ()
showMap l xs = do
  i <- runProcessWithInput "dunstify" [legend, "-p"] ""
  submap $ mkKeymap l keyMap
  spawn $ "dunstify -C " ++ i
  where
    legend :: String
    legend = intercalate "\n" [key ++ " " ++ name | (key,name,_) <- xs]
    keyMap :: [(String, X ())]
    keyMap = [(key,cmd) | (key,_,cmd) <- xs]

emacsStyleKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
emacsStyleKeys l = M.union
  (mkKeymap l $
    [ ("M-<Return>", spawn $ terminal l)
    , ("M-S-c", kill)
    --windows
    , ("M-n", windows W.focusDown)
    , ("M-S-n", windows W.focusUp)
    , ("M-m", windows W.swapDown)
    , ("M-S-m", windows W.swapUp)
    , ("M-w", sendMessage Expand)
    , ("M-S-w", sendMessage Shrink)
    , ("M-l", sendMessage NextLayout)
    , ("M-t", withFocused $ windows . W.sink)
    --system
    , ("M-p", spawn "mpc toggle")
    , ("M-v", spawn "Volume up" >> spawn "panel_volume +")
    , ("M-S-v", spawn "Volume down" >> spawn "panel_volume -")
    , ("M-<F3>", spawn "amixer set Master toggle")
    , ("M-s", submap sysKeys)
    , ("M-a", appsKeys)
    , ("M-S-t", testMap)
    --applications
    , ("M-S-z", spawn "clip_key")
    , ("M-<Space>", spawn "rofi-dmenu")
    , ("M-f", spawn "rofi-filer")
    , ("<XF86AudioPlay>", spawn "timeout 1 playerctl play-pause")
    ])
  (workspaceKeys l)
  where
    sysKeys = mkKeymap l $
      [ ("s", spawn "scrot -s")
      , ("d", spawn $ "echo '?' > " ++ status_pipe)
      , ("l", spawn "gnome-screensaver-command -l || slock")
      , ("h", spawn "boseqc.sh")
      , ("S-s", spawn "systemctl suspend")
      , ("S-h", spawn "systemctl hibernate")
      , ("m", spawn "toggle_mouse.sh")
      ]
      ++
      [(show i, brightness i)| i <- [0..9]]
    appsKeys = showMap l $
      [ ("e", "emacsclient", spawn "e")
      , ("q", "qutebrowser", spawn "/usr/bin/qutebrowser || qutebrowser")
      , ("s", "slack", spawn "slack")
      ]
    testMap = showMap l $
                [ ("a", "ayy", spawn "notify-send 'ayy'")
                , ("b", "bee", spawn "notify-send 'bee")
		]

workspaceKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
workspaceKeys conf@XConfig {XMonad.modMask = modMask} =
  let mm = modMask
      sm = shiftMask
      m1m = mod1Mask
  in M.fromList $
    -- @greedyView@ will move the given workspace to the current screen, while
    -- @view@ will simply move to another screen if the workspace is there.
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. mm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.view, 0), (W.shift, sm)]]
    ++
    -- mod-{w,e,r} %! Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r} %! Move client to screen 1, 2, or 3
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_e, xK_r, xK_o] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]
brightness x =
  let
    percentage = show $ 10 * if x == 0 then 10 else x
  in do
    spawn $ "notify-send 'Brightness " ++ percentage ++ "%'"
    spawn $ "brightnessctl s " ++ percentage ++ "%"

-- | Copy all windows in the specified workspace to the current workspace
copyWorkspace :: (Eq s, Eq i, Eq a) => i -> W.StackSet i l a s sd -> W.StackSet i l a s sd
copyWorkspace i ss = maybe ss moveall targetWindows
  where
    targetWindows = do
      s <- W.stack =<< find ((==i) . W.tag) (W.workspaces ss)
      return $ W.up s ++ W.down s ++ [W.focus s]
    moveall = foldl (\acc x -> copyWindow x (W.currentTag ss) acc) ss


myManageHook = composeAll
                 [ className =? "URxvt" --> doF W.swapDown
                 , className =? "Alacritty" --> doF W.swapDown
                 , className =? "konsole" --> doF W.swapDown
                 ]
