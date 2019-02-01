import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Util.Run(spawnPipe)
import XMonad.Util.EZConfig(additionalKeys)
import XMonad.Layout.Spacing
import XMonad.Hooks.FadeInactive
import XMonad.Layout.Tabbed
import XMonad.Layout.Fullscreen
import XMonad.Layout.NoBorders
import XMonad.Layout.ThreeColumns
import XMonad.Layout.Accordion
import XMonad.Layout.Circle
import XMonad.Layout.Dwindle
import XMonad.Layout.BinarySpacePartition
import XMonad.Layout.StackTile
import System.IO
import System.Exit
import XMonad.Hooks.SetWMName
import XMonad.Hooks.EwmhDesktops
import Data.Char (isAscii)
import qualified XMonad.Actions.DynamicWorkspaceOrder as DO
import XMonad.Util.EZConfig
import XMonad.Actions.CycleWS 
import Graphics.X11.ExtraTypes.XF86

import qualified Data.Map as M         -- haskell modules
import qualified XMonad.StackSet as W  -- xmonad core
import XMonad.Actions.FloatKeys        -- actions (keyResizeWindow)
import XMonad.Actions.FloatSnap        -- actions (snapMove)

------------------------------------------------------------------------
-- Preferences
-- Thre preferred terminal program
myTerminal = "xterm.sh"

-- The command to lock the screen or show the screensaver.
myScreensaver = "lock.sh"

-- The command to take a selective screenshot, where you select
-- what you'd like to capture on the screen.
mySelectScreenshot = "select-screenshot"

-- The command to take a fullscreen screenshot.
myScreenshot = "shot.sh"

-- The command to use as a launcher, to launch commands that don't have
-- preset keybindings.
-- myLauncher = "$(yeganesh -x -- -fn 'monospace-8' -nb '#000000' -nf '#FFFFFF' -sb '#7C7C7C' -sf '#CEFFAC')"
-- myLauncher = "dmenu_run"
myLauncher = "rofi -show run"

-- Location of your xmobar.hs / xmobarrc
myXmobarrc = "~/.xmonad/xmobar-single.hs"

------------------------------------------------------------------------
-- Workspaces
-- The default number of workspaces (virtual screens) and their names.

myWorkspaces = ["1:TERMINAL","2:WEB","3:CODE","4:MEDIA","5:OTHER"] ++ map show [6..9]

------------------------------------------------------------------------
-- Window rules
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, use
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "Chromium"       --> doShift "2:WEB"
    , className =? "Google-chrome"  --> doShift "2:WEB"
    , className =? "Firefox"        --> doShift "2:WEB"
    , className =? "Emacs"          --> doShift "3:CODE"
    ]

------------------------------------------------------------------------
-- Layouts
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
mySpacing i = spacingRaw True (Border i i i i) True (Border i i i i) True
ss = mySpacing 3
myLayout = avoidStruts $ ss $
    (ss $ Tall 1 (3/100) (1/2)) |||
    tabbed shrinkText tabConfig |||
    (ss $ Mirror (Tall 1 (3/100) (1/2))) |||
    (ss $ Dwindle R CW (3/2) (11/10)) |||
    (ss $ Squeeze D (3/2) (11/10))

------------------------------------------------------------------------
-- Colors and borders
-- Currently based on the ir_black theme.
--
myNormalBorderColor  = "#222222"
myFocusedBorderColor = "#aaaaaa"

-- Colors for text and backgrounds of each tab when in "Tabbed" layout.
tabConfig = defaultTheme {
    activeBorderColor = "#444444",
    activeTextColor = "#f2f2f2",
    activeColor = "#666666",
    inactiveBorderColor = "#000000",
    inactiveTextColor = "#cccccc",
    inactiveColor = "#000000"
}

-- Color of current window title in xmobar.
xmobarTitleColor = "#adadad"

-- Color of current workspace in xmobar.
xmobarCurrentWorkspaceColor = "#00e27c"

-- Width of the window border in pixels.
myBorderWidth = 2

------------------------------------------------------------------------
-- Key bindings
--
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
myModMask = mod4Mask

myKeys conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
  ----------------------------------------------------------------------
  -- Custom key bindings
  --

  -- Start a terminal.  Terminal to start is specified by myTerminal variable.
  [ ((modMask .|. shiftMask, xK_Return),
     spawn $ XMonad.terminal conf)

  -- Lock the screen using command specified by myScreensaver.
  , ((modMask .|. controlMask, xK_l),
     spawn myScreensaver)

  -- Spawn the launcher using command specified by myLauncher.
  -- Use this to launch programs without a key binding.
  , ((modMask, xK_p),
     spawn myLauncher)

  -- Take a selective screenshot using the command specified by mySelectScreenshot.
  , ((modMask .|. shiftMask, xK_p),
     spawn mySelectScreenshot)

  -- Take a full screenshot using the command specified by myScreenshot.
  , ((modMask .|. controlMask .|. shiftMask, xK_p),
     spawn myScreenshot)

  -- Mute volume.
  , ((0, xF86XK_AudioMute),
     spawn "mixer vol 0")

  -- Decrease volume.
  , ((0, xF86XK_AudioLowerVolume),
     spawn "mixer vol -10")

  -- Increase volume.
  , ((0, xF86XK_AudioRaiseVolume),
     spawn "mixer vol +10")

  -- Mute volume.
  , ((modMask .|. controlMask, xK_m),
     spawn "mixer vol 0")

  -- Decrease volume.
  , ((modMask .|. controlMask, xK_j),
     spawn "mixer vol -10")

  -- Increase volume.
  , ((modMask .|. controlMask, xK_k),
     spawn "mixer vol +10")

  -- Audio previous.
  , ((0, 0x1008FF16),
     spawn "")

  -- Play/pause.
  , ((0, 0x1008FF14),
     spawn "")

  -- Audio next.
  , ((0, 0x1008FF17),
     spawn "")
 
  -- -------------------------------------------------------------------
  -- "Standard" xmonad key bindings
  --

  -- Close focused window.
  , ((modMask .|. shiftMask, xK_c),
     kill)

  -- Cycle through the available layout algorithms.
  , ((modMask, xK_space),
     sendMessage NextLayout)

  --  Reset the layouts on the current workspace to default.
  , ((modMask .|. shiftMask, xK_space),
     setLayout $ XMonad.layoutHook conf)

  -- Resize viewed windows to the correct size.
  , ((modMask, xK_n),
     refresh)

  -- Move focus to the next window.
  , ((modMask, xK_Tab),
     windows W.focusDown)

  -- Move focus to the next window.
  , ((modMask, xK_j),
     windows W.focusDown)

  -- Move focus to the previous window.
  , ((modMask, xK_k),
     windows W.focusUp  )

  -- Move focus to the master window.
  , ((modMask, xK_m),
     windows W.focusMaster  )

  -- Swap the focused window and the master window.
  , ((modMask, xK_Return),
     windows W.swapMaster)

  -- Swap the focused window with the next window.
  , ((modMask .|. shiftMask, xK_j),
     windows W.swapDown  )

  -- Swap the focused window with the previous window.
  , ((modMask .|. shiftMask, xK_k),
     windows W.swapUp    )

  -- Shrink the master area.
  , ((modMask, xK_h),
     sendMessage Shrink)

  -- Expand the master area.
  , ((modMask, xK_l),
     sendMessage Expand)

  -- Push window back into tiling.
  , ((modMask, xK_t),
     withFocused $ windows . W.sink)

  -- Increment the number of windows in the master area.
  , ((modMask, xK_comma),
     sendMessage (IncMasterN 1))

  -- Decrement the number of windows in the master area.
  , ((modMask, xK_period),
     sendMessage (IncMasterN (-1)))

  -- Toggle the status bar gap.
  -- TODO: update this binding with avoidStruts, ((modMask, xK_b),

  -- Quit xmonad.
  , ((modMask .|. shiftMask, xK_q),
     io (exitWith ExitSuccess))

  -- Restart xmonad.
  , ((modMask, xK_q),
     restart "xmonad" True)
  ]
  ++

  -- mod-[1..9], Switch to workspace N
  -- mod-shift-[1..9], Move client to workspace N
  [((m .|. modMask, k), windows $ f i)
      | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
      , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
  ++

  -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]


------------------------------------------------------------------------
-- Mouse bindings
--
-- Focus rules
-- True if your focus should follow your mouse cursor.
--
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True

myMouseBindings (XConfig {XMonad.modMask = modm}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modm, button1), (\w -> focus w >> mouseMoveWindow w
                                       >> windows W.shiftMaster))

    -- mod-button2, Sink the window back to the grid.
    , ((modm, button2), (\w -> withFocused $ windows . W.sink))

    -- mod-button3, Set the window to floating mode and resize by dragging
    , ((modm, button3), (\w -> focus w >> mouseResizeWindow w
                                       >> windows W.shiftMaster))
    ]

------------------------------------------------------------------------
-- Startup hook
--
myStartupHook = do
                spawn "compton"

------------------------------------------------------------------------
-- Combine it all together
-- A structure containing your configuration settings, overriding
-- fields in the default config. Any you don't override, will
-- use the defaults defined in xmonad/XMonad/Config.hs
--
-- No need to modify this.
--
defaults = defaultConfig {
    -- simple stuff
    terminal           = myTerminal,
    focusFollowsMouse  = myFocusFollowsMouse,
    borderWidth        = myBorderWidth,
    modMask            = myModMask,
    workspaces         = myWorkspaces,
    normalBorderColor  = myNormalBorderColor,
    focusedBorderColor = myFocusedBorderColor,

    -- key bindings
    keys               = myKeys,
    mouseBindings      = myMouseBindings,

    -- hooks, layouts
    layoutHook         = smartBorders $ myLayout,
    manageHook         = myManageHook,
    startupHook        = myStartupHook
}

------------------------------------------------------------------------
-- Run Xmonad
main = do
    xmproc <- spawnPipe ("xmobar " ++ myXmobarrc)
    xmonad $ ewmh defaults { 
      logHook = dynamicLogWithPP $ xmobarPP {
          ppOutput = hPutStrLn xmproc
        , ppTitle = filter isAscii . xmobarColor xmobarTitleColor "" . shorten 50
        , ppCurrent = xmobarColor xmobarCurrentWorkspaceColor ""
        , ppSep = "   "
        , ppSort = DO.getSortByOrder
        }
        , startupHook = docksStartupHook <+> setWMName "LG3D"
        , manageHook = manageDocks <+> myManageHook
        , handleEventHook = docksEventHook
    } `additionalKeysP`
      [
        ("M-C-<R>",   DO.swapWith Next NonEmptyWS)
      , ("M-C-<L>",   DO.swapWith Prev NonEmptyWS)
      , ("M-S-<R>",   DO.shiftTo Next HiddenNonEmptyWS)
      , ("M-S-<L>",   DO.shiftTo Prev HiddenNonEmptyWS)
      , ("M-<R>",     DO.moveTo Next HiddenNonEmptyWS)
      , ("M-<L>",     DO.moveTo Prev HiddenNonEmptyWS)
      ]
