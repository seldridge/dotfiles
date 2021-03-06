-- Imports.
import XMonad
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.EwmhDesktops
import XMonad.Util.Scratchpad
import XMonad.Util.Dmenu

import System.Exit

import Control.Monad

import qualified XMonad.StackSet as W
import qualified Data.Map        as M

-- The main function.
main = xmonad =<< statusBar myBar myPP toggleStrutsKey myConfig

-- Command to launch the bar.
myBar = "xmobar"

-- Terminal name
myTerminal = "urxvtc -name urxvt-scratchpad"

-- Custom PP, configure it as you like. It determines what is being written to the bar.
myPP = xmobarPP { ppCurrent = xmobarColor "#74c476" "" . wrap "[" "]"
                , ppVisible = wrap "<" ">"
                , ppHidden = id
                , ppHiddenNoWindows = id
                }

-- Key binding to toggle the gap for the bar.
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

-- Setup
manageScratchPad :: ManageHook
manageScratchPad = scratchpadManageHook (W.RationalRect l t w h)
  where
    h = 0.95        -- terminal height
    w = 0.95        -- terminal width
    t = (1 - h) / 2 -- distance from top edge
    l = (1 - w) / 2 -- distance from left edge

-- Utility to give a user a yes/no prompt via dmenu to confirm that
-- they want to execute some action
confirm :: String -> X () -> X ()
confirm m f = do
  result <- dmenu [m, "yes", "no"]
  when (result == "yes") f

-- Key Bindings
myKeys conf@(XConfig {XMonad.modMask = modm}) = M.fromList $

    -- launch a terminal
    [ ((modm .|. shiftMask, xK_Return), spawn $ XMonad.terminal conf)

    -- launch dmenu
    , ((modm,               xK_p     ), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")

    -- launch dmenu
    , ((modm .|. shiftMask, xK_p     ), spawn "passmenu --type")

    -- close focused window
    , ((modm .|. shiftMask, xK_c     ), kill)

     -- Rotate through the available layout algorithms
    , ((modm,               xK_space ), sendMessage NextLayout)

    --  Reset the layouts on the current workspace to default
    , ((modm .|. shiftMask, xK_space ), setLayout $ XMonad.layoutHook conf)

    -- Resize viewed windows to the correct size
    , ((modm,               xK_n     ), refresh)

    -- Move focus to the next window
    , ((modm,               xK_Tab   ), windows W.focusDown)

    -- Move focus to the next window
    , ((modm,               xK_j     ), windows W.focusDown)

    -- Move focus to the previous window
    , ((modm,               xK_k     ), windows W.focusUp  )

    -- Move focus to the master window
    , ((modm,               xK_m     ), windows W.focusMaster  )

    -- Swap the focused window and the master window
    , ((modm,               xK_Return), windows W.swapMaster)

    -- Swap the focused window with the next window
    , ((modm .|. shiftMask, xK_j     ), windows W.swapDown  )

    -- Swap the focused window with the previous window
    , ((modm .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- Shrink the master area
    , ((modm,               xK_h     ), sendMessage Shrink)

    -- Expand the master area
    , ((modm,               xK_l     ), sendMessage Expand)

    -- Push window back into tiling
    , ((modm,               xK_t     ), withFocused $ windows . W.sink)

    -- Increment the number of windows in the master area
    , ((modm              , xK_comma ), sendMessage (IncMasterN 1))

    -- Deincrement the number of windows in the master area
    , ((modm              , xK_period), sendMessage (IncMasterN (-1)))

    -- Quit xmonad
    , ((modm .|. shiftMask, xK_q     ), confirm "Do you really want to exit?" (io (exitWith ExitSuccess)))

    -- Restart xmonad
    , ((modm              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")

    -- Scratchpad
    , ((modm, xK_grave), scratchpadSpawnActionTerminal myTerminal)

    -- Automatic xrandr handling
    , ((modm, xK_F11), spawn "xrandr_dwim")

    -- Screen locking
    , ((modm .|. mod1Mask, xK_l),               spawn "xautolock -locknow")
    , ((modm .|. mod1Mask .|. shiftMask, xK_l), spawn "watson stop & ssh-kill & xautolock -locknow & systemctl suspend")

    -- Screen capture
    , ((modm, xK_Print), spawn "maim -s $HOME/screenshots/$(date +%Y-%m-%d-%T).png")
    , ((modm .|. shiftMask, xK_Print), spawn "maim $HOME/screenshots/$(date +%Y-%m-%d-%T).png")

    ]
    ++

    --
    -- mod-[1..9], Switch to workspace N
    --
    -- mod-[1..9], Switch to workspace N
    -- mod-shift-[1..9], Move client to workspace N
    --
    [((m .|. modm, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_1 .. xK_9]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++

    --
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modm, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-- Main configuration, override the defaults to your liking.
-- myConfig = ewmh defaultConfig
myConfig = defaultConfig
  { modMask = mod4Mask
  , terminal = myTerminal
  -- , workspaces = [ "一/1", "二/2", "三/3", "四/4", "五/5", "六/6", "七/7", "八/8", "九/9"]
  , workspaces = [ "1", "2", "3", "4", "5", "6", "7", "8", "9"]
  , keys = myKeys
  , manageHook = manageScratchPad
  , logHook = dynamicLogString myPP >>= xmonadPropLog
  }
