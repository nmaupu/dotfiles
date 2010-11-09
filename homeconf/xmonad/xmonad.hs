--
-- Tiago Rodrigues xmonad config file for xmonad-darcs derived from these
-- official configuration template
--
-- Some of the original template comments and contents are left as a guide
-- not only for me, but others as well.
--  
import XMonad
import XMonad.Layout.MosaicAlt
import XMonad.Layout.Grid
import XMonad.Layout.PerWorkspace
import XMonad.Hooks.SetWMName
import XMonad.Actions.UpdatePointer
import XMonad.Layout.NoBorders
import XMonad.Layout.Tabbed
import XMonad.Layout.Dishes
import XMonad.Layout.StackTile
import XMonad.Layout.MagicFocus
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.EwmhDesktops
import XMonad.Hooks.ManageDocks
import XMonad.Config.Gnome
import XMonad.Actions.CycleWS
import XMonad.Actions.Plane
import System.Exit
import XMonad.Prompt
import XMonad.Prompt.Workspace

-- Dzen2
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
-- spawnPipe
import XMonad.Util.Run

import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
 


-- The preferred terminal program, which is used in a binding below and by
-- certain contrib modules.
--
myTerminal      = "urxvt"
 
-- Width of the window border in pixels.
--
myBorderWidth   = 2
 
-- modMask lets you specify which modkey you want to use. The default
-- is mod1Mask ("left alt").  You may also consider using mod3Mask
-- ("right alt"), which does not conflict with emacs keybindings. The
-- "windows key" is usually mod4Mask.
--
-- Although it's advised not to use Alt with Gnome, i decided to use it and
-- tweak the few xmonad shortcuts that could conflict with gnome, because
-- i've always used shift+alt+keys for desktop changing. I got rid of the
-- default xmonad shortcuts for modMask + desktop number and use only the keys
-- for changing. The reasons for not using the windows key are mostly because
-- of some RSI i've developed because of too many ctrl+alt shortcuts (and ctrl
-- and alt were too close on my old laptop keyboard)
--
myModMask       = mod4Mask
 
-- The mask for the numlock key. Numlock status is "masked" from the
-- current modifier status, so the keybindings will work with numlock on or
-- off. You may need to change this on some systems.
--
-- You can find the numlock modifier by running "xmodmap" and looking for a
-- modifier with Num_Lock bound to it:
--
-- > $ xmodmap | grep Num
-- > mod2        Num_Lock (0x4d)
--
-- Set numlockMask = 0 if you don't have a numlock key, or want to treat
-- numlock status separately.
--
myNumlockMask   = mod2Mask
 
-- The default number of workspaces (virtual screens) and their names.
-- By default we use numeric strings, but any string may be used as a
-- workspace name. The number of workspaces is determined by the length
-- of this list.
--
--myWorkspaces    = ["1:Prod","2:BLR","3:Misc","4:Test","5:Web","6:Com","7","8","9","10","11","12"]
myWorkspaces    = [" α ", " β " ," γ ", " δ ", " ε ", " ζ ", " η ", " θ ", " ι "] 

 
-- Border colors for unfocused and focused windows, respectively.
--
myNormalBorderColor  = "#dddddd"
myFocusedBorderColor = "#0099ff"
 
------------------------------------------------------------------------
-- Key bindings. Add, modify or remove key bindings here.
--
myKeyBindings conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- Increment the number of windows in the master area
    [ ((modMask .|. shiftMask , xK_p ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
    , ((modMask .|. shiftMask , xK_o), sendMessage (IncMasterN (-1)))
    -- launch a terminal
    , ((modMask, xK_Return), spawn $ XMonad.terminal conf)
    -- launch dmenu
    , ((modMask,  xK_p), spawn "exe=`dmenu_path | dmenu` && eval \"exec $exe\"")
    -- close focused window 
    , ((modMask .|. shiftMask, xK_c     ), kill)
    -- Rotate through the available layout algorithms
    , ((modMask, xK_space ), sendMessage NextLayout)
    -- Resize viewed windows to the correct size
    , ((modMask .|. shiftMask, xK_n     ), refresh)
    -- Move focus to the next / previous window
    , ((modMask, xK_Right), windows W.focusDown)
    , ((modMask, xK_Left), windows W.focusUp  )
    -- Swap the focused window and the master window
    , ((modMask .|. shiftMask, xK_m), windows W.swapMaster)
    -- Swap the focused window with the next window
    , ((modMask, xK_m     ), windows W.swapDown  )
    -- Swap the focused window with the previous window
    , ((modMask, xK_l     ), windows W.swapUp)
    -- screensaver
    , ((mod1Mask .|. controlMask, xK_l     ), spawn "xscreensaver-command -lock")
    -- Swap the focused window with the previous window
    -- , ((modMask .|. shiftMask, xK_Return     ), windows W.swapUp    )
    -- Shrink the master area
    , ((modMask, xK_Down     ), sendMessage Shrink)
    -- Expand the master area
    , ((modMask, xK_Up  ), sendMessage Expand)
    -- Reset the layout
    , ((modMask .|. shiftMask .|. controlMask, xK_space), sendMessage resetAlt)
    -- Push window back into tiling
    , ((modMask, xK_t     ), withFocused $ windows . W.sink)
    -- Increment the number of windows in the master area
--    , ((modMask .|. shiftMask , xK_o ), sendMessage (IncMasterN 1))
    -- Deincrement the number of windows in the master area
--    , ((modMask .|. shiftMask , xK_p), sendMessage (IncMasterN (-1)))
    -- Gnome's Print screen functionality seems borked when using xmonad, so
    -- we need this
    , ((modMask ,              xK_Print ), spawn "exe=`gnome-screenshot` && eval \"exec $exe\"")
    -- Restart xmonad
    --, ((modMask .|. shiftMask , xK_q), spawn "exe=`pkill dzen2 && pkill conky` && eval \"exec $exe\" $ "restart "xmonad" True)
    --, ((modMask .|. shiftMask , xK_q), spawn "exe=`pkill dzen2; pkill conky` && eval \"exec $exe\"" >> restart "xmonad" True)
    , ((modMask .|. shiftMask , xK_q), restart "xmonad" True)

    -- Switch workspaces (and move windows) horizontally
    , ((modMask .|. controlMask , xK_Left  ), prevWS )
    , ((modMask .|. controlMask , xK_Right ), nextWS )
    , ((modMask .|. shiftMask .|. controlMask, xK_Left  ), shiftToPrev )
    , ((modMask .|. shiftMask .|. controlMask, xK_Right ), shiftToNext )
    -- Display a workspace switcher
    -- , ((modMask , xK_m     ), workspacePrompt defaultXPConfig (windows . W.view))

    -- Send the focused window to the workspace
    -- , ((modMask .|. shiftMask, xK_m), workspacePrompt defaultXPConfig (windows . W.shift))

    -- Switch screen
    -- , ((modMask .|. mod1Mask, xK_Right), spawn "exe=togglescreen && eval \"exec $exe\"")
    -- , ((modMask .|. mod1Mask, xK_Left), spawn "exe=togglescreen && eval \"exec $exe\"")

    ]
    ++
    -- Switch workspaces (and move windows) vertically
    [((keyMask .|. modMask .|. controlMask, keySym), function (Lines 1) Finite direction)
     | (keySym, direction) <- zip [xK_Left .. xK_Down] $ enumFrom ToLeft
     , (keyMask, function) <- [(0, planeMove), (controlMask .|. shiftMask, planeShift)]
    ]
    ++
    -- mod-{w,e,r}, Switch to physical/Xinerama screens 1, 2, or 3
    -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
    --
    [((m .|. modMask .|. shiftMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_w, xK_e, xK_r] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
    ]
 
 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- mod-button1, Set the window to floating mode and move by dragging
    [ ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
    -- mod-button2, Raise the window to the top of the stack
    , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
    -- mod-button3, performs only a resize of the window, based on which quadrant the mouse is in. 
    , ((modMask, button3), ((\w -> focus w >> Flex.mouseWindow Flex.resize w)))
    -- Scrolls to the next workspace (mouse wheel up)
--    , ((modMask, button4), (\w ->nextWS))
    -- Scrolls to the previous workspace (mouse wheel down)
--    , ((modMask, button5), (\w ->prevWS))
    ]
 
------------------------------------------------------------------------
-- Layouts:

-- I don't need theses option now.. Perhaps in a while...
 
-- You can specify and transform your layouts by modifying these values.
-- If you change layout bindings be sure to use 'mod-shift-space' after
-- restarting (with 'mod-q') to reset your layout state to the new
-- defaults, as xmonad preserves your old layout settings by default.
--
-- The available layouts.  Note that each layout is separated by |||,
-- which denotes layout choice.
--
--mosaic = MosaicAlt M.empty
full = noBorders Full

--myLayout = ewmhDesktopsLayout $ (avoidStruts(tiled ||| Mirror tiled ||| StackTile 1 (3/100) (1/2) ||| full) ||| full)
myLayout = avoidStruts(tiled ||| Mirror tiled ||| StackTile 1 (3/100) (1/2) ||| full) ||| full
  where
 -- default tiling algorithm partitions the screen into two panes
     tiled   = Tall nmaster delta ratio
 -- The default number of windows in the master pane
     nmaster = 1
 -- Default proportion of screen occupied by master pane
     ratio   = 6/10
 -- Percent of screen to increment by when resizing panes
     delta   = 3/100


--myLayout = ewmhDesktopsLayout
-- $  tiled ||| Mirror tiled ||| Full
 

------------------------------------------------------------------------
-- Window rules:
 
-- Execute arbitrary actions and WindowSet manipulations when managing
-- a new window. You can use this to, for example, always float a
-- particular program, or have a client always appear on a particular
-- workspace.
--
-- To find the property name associated with a program, us
-- > xprop | grep WM_CLASS
-- and click on the client you're interested in.
--
-- To match on the WM_NAME, you can use 'title' in the same way that
-- 'className' and 'resource' are used below.
--
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , title =? "GNU Image Manipulation Program" --> doFloat
    , title =? "GIMP"                  --> doFloat
    , className =? "Do"               --> doIgnore
    , className =? "Gnome-Screenshot" --> doIgnore
    , className =? "Tilda"            --> doFloat
    , className =? "feh"              --> doFloat
    , className =? "Amarokapp"        --> doFloat
    , title     =? "Save a Bookmark"  --> doFloat
    , className =? "Download"         --> doFloat
    , className =? "Extension"        --> doFloat
    , className =? "Qalculate-gtk"    --> doFloat
    , title     =? "VLC media player" --> doFloat
    --, className =? "Terminator"       --> doF (W.shift "terms" )
    --, className =? "Skype"            --> doF (W.shift "skype" )
    --, className =? "Eclipse"          --> doF (W.shift "eclipse" )
    --, className =? "Firefox"          --> doF (W.shift "web" )
    --, className =? "Pidgin"           --> doF (W.shift "pidgin" )
    --, className =? "GWT"              --> doF (W.shift "gwt" )
    --, title =?     "Start Here"       --> doF (W.shift "main" )
    --, title =?     "irssi"            --> doF (W.shift "main" )
    --, className =? "Twhirl"           --> doF (W.shift "main" )
    , title =?     "Brood War"        --> doIgnore
    , resource  =? "desktop_window"   --> doIgnore
    , resource  =? "kdeskt6op"        --> doIgnore ]
        <+> manageDocks
 
-- Whether focus follows the mouse pointer.
myFocusFollowsMouse :: Bool
myFocusFollowsMouse = True
 
 
------------------------------------------------------------------------
-- Status bars and logging
 
-- Perform an arbitrary action on each internal state change or X event.
-- See the 'DynamicLog' extension for examples.
--
--
-- declares the function myLogHook
-- myLogHook :: X()
-- when myLogHook is executed, it execute first the ewmhDesktopsLogHook, then the updatePointer.
-- myLogHook = ewmhDesktopsLogHook 
-- >> updatePointer (Relative 0.5 0.5) place the pointer in the middle of the newly focused window
--
-- Fix for SWING Applications
------------------------------
-- The Java gui toolkit has a hardcoded list of so-called "non-reparenting" window managers. xmonad is not on 
-- this list (nor are many of the newer window managers). Attempts to run Java applications may result in `grey 
-- blobs' where windows should be, as the Java gui code gets confused.
--
-- The cleanest way is to lie to Java about what window manager you are, by using the SetWMName extension to 
-- pretend to be "LG3D"
--
-- However, modules using Hooks.EwmhDesktops, such as Config.Gnome, Config.Desktops, etc. currently setWMName 
-- to "xmonad" on each X Event, thus over-writing the startup name. Till EwmhDesktops is made smarter, use 
-- setWMName in logHook instead:

myLogHook :: X()
myLogHook = setWMName "LG3D" >> dynamicLogXinerama >> updatePointer (Relative 0.5 0.5)

------------------------------------------------------------------------
-- Startup hook
 
-- See above 
myStartupHook = setWMName "LG3D"

myFont = "-xos4-terminus-bold-r-normal-*-12-*-*-*-c-*-iso8859-1"
myIconDir = "~/.xmonad/icons"
myDzenFGColor = "#555555"
myDzenBGColor = ""
myNormalFGColor = "#ffffff"
myNormalBGColor = "#0f0f0f"
myFocusedFGColor = "#0099ff"
myFocusedBGColor = "#0f0f0f"
myUrgentFGColor = "#0099ff"
myUrgentBGColor = "#0077ff"
myIconFGColor = "#777777"
myIconBGColor = ""
mySeperatorColor = "#555555"

myStatusBar = "dzen2 -m -x '0' -y '0' -h '20' -w '1920' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont  ++ "'"
myDzenRight = "~/.xmonad/scripts/loop.sh | dzen2 -fn \"" ++ myFont  ++ "\" -x 1600 -y 0 -h 20 -w 320 -ta r -bg \"" ++ myNormalBGColor  ++ "\" -fg \"" ++ myNormalFGColor  ++ "\" -p -e ''"
--myStatusBar = "dzen2 -m -x '0' -y '0' -h '20' -w '1680' -ta 'l' -fg '" ++ myNormalFGColor ++ "' -bg '" ++ myNormalBGColor ++ "' -fn '" ++ myFont  ++ "'"


-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
    { ppCurrent = wrap ("^fg(" ++ myFocusedFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^fg(" ++ myFocusedFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppVisible = wrap ("^fg(" ++ myNormalFGColor ++ ")^bg(" ++ myFocusedBGColor ++ ")^p()^fg(" ++ myNormalFGColor ++ ")") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppHidden = wrap ("") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId -- don't use ^fg() here!!
    , ppHiddenNoWindows = wrap ("^fg(" ++ myDzenFGColor ++ ")^bg()^p()") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppUrgent = wrap ("^fg(" ++ myUrgentFGColor ++ ")^bg()^p()") "^fg()^bg()^p()" . \wsId -> if (':' `elem` wsId) then drop 2 wsId else wsId
    , ppSep = " ^i(" ++ myIconDir  ++ "/separator.xbm) "
    , ppWsSep = " "
    , ppTitle = dzenColor ("" ++ myNormalFGColor ++ "") "" . wrap "< " " >"
    , ppLayout = dzenColor ("" ++ myNormalFGColor ++ "") "" .
        (\x -> case x of
        "Hinted Full" -> "^fg(" ++ myIconFGColor ++ ")"
        "Hinted ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")"
        "Hinted Mirror ResizableTall" -> "^fg(" ++ myIconFGColor ++ ")"
        _ -> x
        )
    , ppOutput = hPutStrLn h
    }
 
------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
 
-- Run xmonad with the settings you specify. No need to modify this.
--
main = do
        dzen <- spawnPipe myStatusBar
        dzenRight <- spawnPipe myDzenRight
        xmonad $ defaultConfig
         { terminal           = myTerminal,
           focusFollowsMouse  = myFocusFollowsMouse,
           borderWidth        = myBorderWidth,
           modMask            = myModMask,
           numlockMask        = myNumlockMask,
           workspaces         = myWorkspaces,
           normalBorderColor  = myNormalBorderColor,
           focusedBorderColor = myFocusedBorderColor,
           keys               = myKeyBindings,
           mouseBindings      = myMouseBindings,
           layoutHook         = myLayout,
           manageHook         = myManageHook,
           --logHook          = dynamicLogWithPP $ dzenPP { ppOutput = hPutStrLn dzen },
           logHook            = dynamicLogWithPP (myDzenPP dzen),
           startupHook        = myStartupHook
        }
