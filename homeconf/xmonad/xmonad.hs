import qualified XMonad.Actions.FlexibleManipulate as Flex
import qualified XMonad.StackSet as W
import qualified Data.Map        as M
import System.IO (hPutStrLn)
import XMonad
import XMonad.Actions.CycleWS
import XMonad.Actions.DynamicWorkspaces
import XMonad.Actions.UpdatePointer
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.SetWMName
import XMonad.Hooks.UrgencyHook
import XMonad.Layout.Grid
import XMonad.Layout.MosaicAlt
import XMonad.Layout.NoBorders
import XMonad.Layout.ToggleLayouts
import XMonad.Prompt
import XMonad.Util.Run
import XMonad.Util.WorkspaceCompare (getSortByTag)
--import XMonad.Layout.SimpleDecoration
import XMonad.Layout.Tabbed
import XMonad.Hooks.ICCCMFocus


------------------------------------------------------------------------
-- Func
--
addKeyBinding shortcutLeft shortcutRight action xs = ((shortcutLeft, shortcutRight), action) : xs

takeWorkspaces :: Int -> [String] -> [String]
takeWorkspaces = take

addWS' :: l -> W.StackSet [Char] l a sid sd -> W.StackSet [Char] l a sid sd
addWS' l s@(W.StackSet { W.hidden = ws }) = s { W.hidden = W.Workspace (show $ length (W.workspaces s) + 1) l Nothing:ws }

addWS :: X()
addWS = do l <- asks (layoutHook . config)
           windows (addWS' l)


------------------------------------------------------------------------
-- vars
--
altKey          = mod1Mask
winKey          = mod4Mask
numLockKey      = mod2Mask
myTerminal      = "urxvt"
myBorderWidth   = 2
workspacesPool  = map show [1..]
myWorkspaces    = takeWorkspaces 15 workspacesPool
dzenFont        = "-xos4-terminus-bold-r-normal-*-12-*-*-*-*-*-iso8859-15"
iconDir         = ".xmonad/icons"
iconSep         = iconDir ++ "/separator.xbm"
colBG           = "#0f0f0f"
colHidden       = "#555555"
colUrgent       = "#ff0000"
colFocus        = "#0099ff"
colNormal       = "#ffffff"
colBorderNormal = "#dddddd"
colBorderFocus  = "#AA0033"

shellScriptServer = "~/scripts/xmonad-server-connect.sh"
dmenuCommandOpts  = "-p '>' -l 10 -nf '" ++ colNormal  ++ "' -nb '" ++ colBG ++ "' -fn '"++ dzenFont  ++"' -sb '"++ colFocus ++"' -sf '"++ colNormal  ++"'"
dmenuCommandProg  = "dmenu_run " ++ dmenuCommandOpts
dmenuCommandServ  = "dmenu " ++ dmenuCommandOpts
dmenuProg         = "prog=`" ++ dmenuCommandProg  ++ "` && eval \"exec ${prog}\""
dmenuServ         = "param=`"++ shellScriptServer  ++" -l | " ++ dmenuCommandServ  ++ " -b` && eval \""++ shellScriptServer  ++" -e ${param}\""
lxappearance      = "lxappearance"


------------------------------------------------------------------------
-- Key bindings
--

newKeyBindings x = M.union (M.fromList . keyBindings $ x) (keys defaultConfig x)
keyBindings conf@(XConfig {XMonad.modMask = modMask}) =
  addKeyBinding modMask xK_g addWS $
  addKeyBinding modMask xK_v removeWorkspace $
  --addKeyBinding modMask xK_b (selectWorkspace greenXPConfig)  $
  addKeyBinding cModShift xK_p (sendMessage (IncMasterN 1))   $
  addKeyBinding cModShift xK_o (sendMessage (IncMasterN (-1))) $
  -- launch a terminal
  addKeyBinding modMask xK_Return (spawn $ XMonad.terminal conf) $
  -- launch dmenu
  addKeyBinding modMask xK_p (spawn dmenuProg) $
  addKeyBinding cModCtrl xK_p (spawn lxappearance) $
  -- launch dmenu for servers
  addKeyBinding modMask xK_s (spawn dmenuServ) $
  -- Resize viewed windows to the correct size
  addKeyBinding cModShift xK_n refresh $
  -- Move focus to the next / previous window
  addKeyBinding modMask xK_Right (windows W.focusDown) $
  addKeyBinding modMask xK_Left  (windows W.focusUp  ) $
  -- Swap the focused window and the master window
  addKeyBinding cModShift xK_m (windows W.swapMaster) $
  -- Swap the focused window with the next window
  addKeyBinding modMask xK_m (windows W.swapDown) $
  -- Swap the focused window with the previous window
  addKeyBinding modMask xK_l (windows W.swapUp) $
  -- screensaver
  addKeyBinding cCtrlAlt xK_l (mapM_ spawn ["xscreensaver -no-splash", "xscreensaver-command -lock"]) $
  -- Shrink the master area
  addKeyBinding modMask xK_Down (sendMessage Shrink) $
  -- Expand the master area
  addKeyBinding modMask xK_Up (sendMessage Expand) $
  -- set window fullscreen
  addKeyBinding modMask xK_f (sendMessage ToggleLayout) $
  -- focus urgent window
  addKeyBinding modMask xK_u focusUrgent $
  -- Reset the layout
  addKeyBinding cModCtrlShift xK_space (sendMessage resetAlt) $
  addKeyBinding modMask xK_Print (spawn "exe=`gnome-screenshot` && eval \"exec $exe\"") $
  -- Restart xmonad, does not work with old version
  --addKeyBinding modMask xK_q (mapM_ spawn ["pgrep -f loop.sh | xargs kill -9", "xmonad --recompile"]) $
  -- Switch workspaces (and move windows) horizontally
  addKeyBinding cModCtrl      xK_Left  prevWS      $
  addKeyBinding cModCtrl      xK_Right nextWS      $
  addKeyBinding cModCtrl      xK_Up    toggleWS    $
  addKeyBinding cModCtrl      xK_Down  toggleWS    $
  addKeyBinding cModCtrlShift xK_Left  (shiftToPrev >> prevWS) $
  addKeyBinding cModCtrlShift xK_Right (shiftToNext >> nextWS) $
  -- mod-shift-{w,e,r}, Move client to screen 1, 2, or 3
  ([((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
      | (key, sc) <- zip [xK_z, xK_e, xK_r] [0..]
      , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]
   ]
   ++
   [((m .|. modMask, k), windows $ f i)
       | (i, k) <- zip (workspaces conf) numAzerty,
         (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]])
  where       
    cModCtrl      = modMask   .|. controlMask
    cModShift     = modMask   .|. shiftMask
    cCtrlShift    = shiftMask .|. controlMask
    cCtrlAlt      = altKey    .|. controlMask
    cModCtrlShift = cModCtrl  .|. shiftMask
    numAzerty       = [0x26,0xe9,0x22,0x27,0x28,0x2d,0xe8,0x5f,0xe7,0xe0] ++ [xK_F1..xK_F12]

 
------------------------------------------------------------------------
-- Mouse bindings: default actions bound to mouse events
--
myMouseBindings (XConfig {XMonad.modMask = modMask}) = M.fromList $
    [ 
      -- Set the window to floating mode and move by dragging
      ((modMask, button1), (\w -> focus w >> mouseMoveWindow w))
      -- Raise the window to the top of the stack
      , ((modMask, button2), (\w -> focus w >> windows W.swapMaster))
      -- Performs only a resize of the window, based on which quadrant the mouse is in. 
      , ((modMask, button3), ((\w -> focus w >> Flex.mouseWindow Flex.resize w)))
    ]
 
------------------------------------------------------------------------
-- Layouts:
--
full = noBorders Full
winDecoTabbed = tabbed shrinkText defaultTheme
layouts = avoidStruts(Grid ||| tiled ||| Mirror tiled ||| full)
  where
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio
    -- The default number of windows in the master pane
    nmaster = 1
    -- Default proportion of screen occupied by master pane
    ratio   = 1/10
    -- Percent of screen to increment by when resizing panes
    delta   = 10/100
myLayout = (toggleLayouts $ avoidStruts winDecoTabbed) $ layouts


------------------------------------------------------------------------
-- Window rules:
--
myManageHook = composeAll
    [ className =? "MPlayer"          --> doFloat
    , title =? "GNU Image Manipulation Program" --> doFloat
    , title =? "GIMP"                 --> doFloat
    , className =? "Do"               --> doIgnore
    , className =? "Tilda"            --> doFloat
    , title     =? "VLC media player" --> doFloat
    --, className =? "Firefox"        --> doF (W.shift $ myWorkspaces!!0 )
    --, className =? "Firefox"        --> doF (W.shift $ myWorkspaces!!0 )
    , className =? "Skype"            --> doF (W.shift $ myWorkspaces!!13 )
    ]
        <+> manageDocks
 
------------------------------------------------------------------------
-- Status bars and logging
-- takeTopFocus is useful for java app focus
-- https://gist.github.com/markhibberd/636125/raw/11713d338e98a9dd5d126308218067a1628480df/xmonad-focus-wire.hs
-- http://xmonad.org/xmonad-docs/xmonad-contrib/XMonad-Hooks-ICCCMFocus.html
myLogHook :: X()
myLogHook = takeTopFocus >> setWMName "LG3D" >> dynamicLogXinerama >> updatePointer (Relative 0.5 0.5)

myStartupHook = setWMName "LG3D"
myStatusBar   = "dzen2 -m -x 0 -y 0 -h 20 -w 1130 -ta l -fg '" ++ colNormal ++ "' -bg '" ++ colBG ++ "' -fn '" ++ dzenFont  ++ "'"
myDzenRight   = "/home/nmaupu/.xmonad/scripts/loop.sh | dzen2 -fn '" ++ dzenFont  ++ "' -x 1130 -y 0 -h 20 -w 466 -ta r -bg '" ++ colBG  ++ "' -fg '" ++ colNormal  ++ "' -p -e ''"

-- dynamicLog pretty printer for dzen:
myDzenPP h = defaultPP
  { ppCurrent         = dzenColor colFocus colBG
  , ppVisible         = dzenColor colNormal colBG
  , ppHiddenNoWindows = dzenColor colHidden ""
  , ppUrgent          = dzenColor colUrgent ""
  , ppTitle           = dzenColor colNormal "" . wrap "< " " >"
  , ppWsSep           = "  "
  , ppSep             = " ^i(" ++ iconSep  ++ ") "
  , ppOutput          = hPutStrLn h
  }


------------------------------------------------------------------------
-- Now run xmonad with all the defaults we set up.
--
main = do
  dzen      <- spawnPipe myStatusBar
  dzenRight <- spawnPipe myDzenRight
  xmonad $ withUrgencyHook dzenUrgencyHook { args = ["-bg", "darkgreen", "-xs", "1"] } $ defaultConfig
    { terminal           = myTerminal,
      focusFollowsMouse  = True,
      borderWidth        = myBorderWidth,
      modMask            = winKey,
      --numlockMask        = numLockKey,
      workspaces         = myWorkspaces,
      normalBorderColor  = colBorderNormal,
      focusedBorderColor = colBorderFocus,
      keys               = newKeyBindings,
      mouseBindings      = myMouseBindings,
      layoutHook         = myLayout,
      manageHook         = myManageHook,
      logHook            = dynamicLogWithPP $ myDzenPP dzen,
      startupHook        = myStartupHook
    }
