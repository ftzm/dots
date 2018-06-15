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
import System.IO
import Data.Maybe
import Data.List

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
  spawn "pipestatus"
  xmonad $ myConfig

toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

myConfig = ewmh defaultConfig
    { terminal           = myTerminal
    , modMask            = myModMask
    , borderWidth        = myBorderWidth
    , focusedBorderColor = "#bdae93"
    , normalBorderColor  = "#504945"
    , keys               = myKeys
    , logHook            = myLogHook --statusHandle
    , layoutHook         = myLayouts
    , manageHook         = myManageHook <+> manageDocks
    , workspaces         = myWorkspaces
    , handleEventHook    = handleEventHook defaultConfig <+> fullscreenEventHook
    }

-- TODO: organize this better
writeFileLn f s = writeFile f $ "x;" ++ s ++ "\n"

myLogHook = dynamicLogWithPP $ def { ppOutput = writeFileLn "/tmp/statuspipe.fifo" }

myTabsTheme = def
  { fontName            = "xft:Fira Code:medium:size=14"
  , activeColor         = "#282828"
  , activeTextColor     = "#ebdbb2"
  , inactiveColor       = "#1d2021"
  , inactiveTextColor   = "#a89984"
  , activeBorderColor   = "#282828"
  , inactiveBorderColor = "#282828"
  , decoHeight          = 30
  }

myWorkspaces = [ "main"
               , "web"
               , "dev"
               , "term"
               , "mus"
               , "6"
               , "7"
               , "8"
               , "9"
               ]

myTerminal    = "urxvt"
myModMask     = mod4Mask -- Win key or Super_L
myBorderWidth = 1

--spacing 2 adds 2px spacing around all windows in all layouts
myLayouts = avoidStruts
          $ (smartBorders
          $ rT
          ||| Mirror rT
          ||| Full
          ||| emptyBSP
          ||| (tabbedBottom shrinkText myTabsTheme))
--myLayouts = rT ||| Mirror rT ||| Full ||| emptyBSP ||| tabbed shrinkText myTabsTheme
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

layoutRenamer :: String -> String
layoutRenamer x = case x of
  "ResizableTall"          -> "side"
  "Mirror ResizableTall"   -> "stack"
  "Full"                   -> "max"
  "BSP"                    -> "bsp"
  "Tabbed Bottom Simplest" -> "tabbed"
  x                        -> x

myKeys :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
myKeys conf@XConfig {XMonad.modMask = modMask} =
  let mm = modMask
      sm = shiftMask
      m1m = mod1Mask
  in M.fromList $
    -- launching and killing programs
    [ ((mm, xK_Return), spawn $ XMonad.terminal conf) -- %! Launch terminal
    , ((mm, xK_space),  spawn "my_dmenu.sh")          -- %! Launch dmenu
    , ((mm .|. sm, xK_q),      kill)                         -- %! Close the focused window

    , ((mm, xK_Tab ), sendMessage NextLayout) -- %! Rotate through the available layout algorithms
    , ((mm .|. sm, xK_space ), setLayout $ XMonad.layoutHook conf) -- %!  Reset the layouts on the current workspace to default

    , ((mm, xK_n), refresh) -- %! Resize viewed windows to the correct size

    -- move focus up or down the window stack
    , ((mm, xK_j), windows W.focusDown)   -- %! Move focus to the next window
    , ((mm, xK_k), windows W.focusUp)     -- %! Move focus to the previous window
    , ((mm, xK_m), windows W.focusMaster) -- %! Move focus to the master window

    -- modifying the window order
    , ((mm .|. m1m, xK_m), windows W.swapMaster) -- %! Swap the focused window and the master window
    , ((mm .|. m1m, xK_j), windows W.swapDown  ) -- %! Swap the focused window with the next window
    , ((mm .|. m1m, xK_k), windows W.swapUp    ) -- %! Swap the focused window with the previous window

    -- resizing the master/slave ratio
    , ((mm, xK_h), sendMessage Shrink) -- %! Shrink the master area
    , ((mm, xK_l), sendMessage Expand) -- %! Expand the master area

    -- floating layer support
    , ((mm, xK_t), withFocused $ windows . W.sink) -- %! Push window back into tiling

    -- increase or decrease number of windows in the master area
    , ((mm , xK_comma),  sendMessage (IncMasterN 1)) -- %! Increment the number of windows in the master area
    , ((mm , xK_period), sendMessage (IncMasterN (-1))) -- %! Deincrement the number of windows in the master area

    -- quit, or restart
    , ((mm .|. sm .|. m1m, xK_q), io exitSuccess) -- %! Quit xmonad
    --, ((mm .|. sm .|. m1m, xK_r), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad
    , ((mm .|. sm .|. m1m, xK_r), spawn "if type xmonad; then xmonad --recompile && xmonad --restart; else xmessage xmonad not in \\$PATH: \"$PATH\"; fi") -- %! Restart xmonad

    --ResizableTile keys
    , ((mm, xK_a), sendMessage MirrorShrink)
    , ((mm, xK_z), sendMessage MirrorExpand)

    --historical
    , ((mm, xK_semicolon), nextMatch History (return True))

    --bsp keys
    , ((mm .|. sm, xK_p),         sendMessage FocusParent)
    , ((mm .|. sm, xK_s),         sendMessage Swap)
    , ((mm .|. sm, xK_r),         sendMessage Rotate)
    , ((mm .|. sm, xK_n),         sendMessage SelectNode)
    , ((mm .|. sm, xK_m),         sendMessage MoveNode)
    , ((mm .|. sm, xK_l),         sendMessage $ ExpandTowards R)
    , ((mm .|. sm, xK_h),         sendMessage $ ExpandTowards L)
    , ((mm .|. sm, xK_j),         sendMessage $ ExpandTowards D)
    , ((mm .|. sm, xK_k),         sendMessage $ ExpandTowards U)
    , ((mm .|. sm .|. m1m, xK_l), sendMessage $ ShrinkFrom R)
    , ((mm .|. sm .|. m1m, xK_h), sendMessage $ ShrinkFrom L)
    , ((mm .|. sm .|. m1m, xK_j), sendMessage $ ShrinkFrom D)
    , ((mm .|. sm .|. m1m, xK_k), sendMessage $ ShrinkFrom U)

    --My desktop keys
    , ((mm, xK_Down),      spawn "panel_volume -")
    , ((mm, xK_Up),        spawn "panel_volume +")
    , ((mm, xK_F3),        spawn "amixer set Master toggle")
    , ((mm, xK_F5),        spawn "xbacklight -dec 10")
    , ((mm, xK_F10),       spawn "scrot -s")
    , ((mm, xK_F6),        spawn "xbacklight -inc 10")
    , ((mm .|. m1m, xK_b), spawn "kbds")
    , ((mm .|. m1m, xK_h), spawn "systemctl hibernate")
    , ((mm, xK_F7),        spawn "playerctl play-pause")
    , ((mm .|. m1m, xK_t), spawn "toggle_mouse.sh")
    , ((mm .|. m1m, xK_r), spawn "konsole -e ranger")
    , ((mm .|. sm, xK_z), spawn "clip_key")
    , ((mm .|. m1m, xK_s), spawn "slock")
    ]
    ++
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
    -- [((m .|. m1m, key), screenWorkspace sc >>= flip whenJust (windows . f))
    --     | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
    --     , (f, m) <- [(W.view, 0), (W.shift, sm)]]

    [((sm .|. m1m, k), windows $ copyWorkspace i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]]

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
                 , className =? "konsole" --> doF W.swapDown
                 ]
