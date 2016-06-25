-- Imports --
-- stuff
import XMonad
import qualified XMonad.StackSet as W
import qualified Data.Map as M
import System.Exit
import XMonad.Util.Run (safeSpawn)
import Graphics.X11.ExtraTypes.XF86
import XMonad.Util.Paste
import XMonad.Prompt
import XMonad.Prompt.Input

-- actions
import XMonad.Actions.GridSelect

-- hooks
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Hooks.UrgencyHook
import XMonad.Hooks.InsertPosition
import XMonad.Hooks.EwmhDesktops

-- layouts
import XMonad.Layout.NoBorders
import XMonad.Layout.ResizableTile
import XMonad.Layout.Renamed
import XMonad.Layout.Tabbed
import XMonad.Layout.Named

-------------------------------------------------------------------------------
-- Main --
main :: IO ()
main = xmonad =<< statusBar cmd pp kb conf
  where
    cmd = "bash -c \"tee >(xmobar -x0) | xmobar -x1\""
    pp = customPP
    kb = toggleStrutsKey
    conf = conf'

-------------------------------------------------------------------------------
-- Configs --
conf' = defaultConfig
	{ workspaces = workspaces'
	, modMask = modMask'
	, borderWidth = borderWidth'
	, normalBorderColor = normalBorderColor'
	, focusedBorderColor = focusedBorderColor'
	, terminal = terminal'
	, keys = keys'
	, layoutHook = layoutHook'
	, handleEventHook = fullscreenEventHook
	}

-------------------------------------------------------------------------------
-- Looks --
-- bar
customPP = defaultPP { ppCurrent = xmobarColor "#C9A34E" "" . wrap "<" ">"
                     , ppHidden = xmobarColor "#93a1a1" ""
                     , ppHiddenNoWindows = xmobarColor "#657b83" ""
                     , ppUrgent = xmobarColor "#dc322f" "" . wrap "[" "]"
                     , ppLayout = xmobarColor "#C9A34E" ""
                     , ppTitle =  xmobarColor "#C9A34E" "" . shorten 80
                     , ppSep = xmobarColor "#429942" "" " | "
                     }

-- borders
borderWidth' = 2
normalBorderColor'  = "#073642"
focusedBorderColor' = "#cb9b00"

-- workspaces
workspaces' = ["1-main", "2-web", "3-mail", "4-dev-doc", "5-dev-code", "6-dev-test", "7", "8", "9"]

-- Layouts --
layoutHook' = tile ||| mtile ||| full
  where
    rt      = ResizableTall 0 (2/100) (1/2) []
    -- normal vertical tile
    tile    = named "[]="   $ smartBorders rt
    -- normal horizontal tile
    mtile   = named "M[]="  $ smartBorders $ Mirror rt
    -- fullscreen without tabs
    full        = named "[ ]"    $ noBorders Full

-------------------------------------------------------------------------------
-- Terminal --
terminal' = "urxvtc"

-------------------------------------------------------------------------------
-- Keys/Button bindings --
-- modmask
modMask' = mod4Mask

myXPConfig = defaultXPConfig { height = 20 }

-- keys
toggleStrutsKey :: XConfig Layout -> (KeyMask, KeySym)
toggleStrutsKey XConfig {XMonad.modMask = modMask} = (modMask, xK_b)

safeShutdown msg =
	if msg == "o"
	then io (exitWith ExitSuccess)
	else spawn ""

keys' :: XConfig Layout -> M.Map (KeyMask, KeySym) (X ())
keys' conf@(XConfig {XMonad.modMask = modMask}) = M.fromList $
    -- launching and killing programs
    [ ((modMask,               xK_Return), safeSpawn (XMonad.terminal conf) [])
    , ((modMask .|. shiftMask, xK_c     ), kill)
    , ((modMask,	       xK_c     ), safeSpawn "iceweasel" ["https://portail-captif.grenet.fr/fs/customwebauth/login.html?switch_url=https://portail-captif.grenet.fr/login.html&redirect=wwww.duckduckgo.com"])
    , ((modMask,	       xK_o     ), spawn "iceweasel")
    , ((modMask,	       xK_p     ), safeSpawn "iceweasel" ["--private-window"])
    , ((modMask,	       xK_i     ), spawn "icedove")
	, ((modMask,		   xK_r	    ), spawn "redshift -l 45:5")
	, ((modMask .|. shiftMask, xK_r ), spawn "pkill -x redshift")

    -- multimedia
    , ((0, xF86XK_AudioRaiseVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1+"])
    , ((0, xF86XK_AudioLowerVolume      ), safeSpawn "amixer" ["-q", "set", "Master", "1-"])
    , ((0, xF86XK_AudioMute             ), safeSpawn "amixer" ["-q", "set", "Master", "toggle"])

    -- floating layer stuff
    , ((modMask,               xK_t     ), withFocused $ windows . W.sink)

    -- refresh
    , ((modMask,               xK_n     ), refresh)

    -- focus
    , ((modMask,               xK_j     ), windows W.focusDown)
    , ((modMask,               xK_k     ), windows W.focusUp)
    , ((modMask,               xK_m     ), windows W.focusMaster)

    -- layouts
    , ((modMask,                xK_space    ), sendMessage NextLayout)
    , ((modMask .|. shiftMask,  xK_space    ), setLayout $ XMonad.layoutHook conf)

    -- swapping
    , ((modMask .|. shiftMask, xK_m     ), windows W.swapMaster)
    , ((modMask .|. shiftMask, xK_j     ), windows W.swapDown  )
    , ((modMask .|. shiftMask, xK_k     ), windows W.swapUp    )

    -- increase or decrease number of windows in the master area
    , ((modMask              , xK_comma ), sendMessage (IncMasterN 1))
    , ((modMask              , xK_semicolon), sendMessage (IncMasterN (-1)))

    -- resizing
    , ((modMask,               xK_h     ), sendMessage Shrink)
    , ((modMask,               xK_l     ), sendMessage Expand)
    , ((modMask .|. shiftMask, xK_h     ), sendMessage MirrorShrink)
    , ((modMask .|. shiftMask, xK_l     ), sendMessage MirrorExpand)

    -- quit, or restart
	, ((modMask,               xK_Escape), spawn "systemctl suspend")
	, ((modMask .|. shiftMask, xK_Escape), spawn "systemctl hybrid-sleep")
    , ((modMask .|. shiftMask, xK_q     ), inputPrompt myXPConfig "éteindre ?(o/n)" ?+ safeShutdown)
    , ((modMask              , xK_q     ), spawn "xmonad --recompile; xmonad --restart")
    ]
    ++
    -- mod-[1..9] %! Switch to workspace N
    -- mod-shift-[1..9] %! Move client to workspace N
    [((m .|. modMask, k), windows $ f i)
        | (i, k) <- zip (XMonad.workspaces conf) [xK_ampersand, xK_eacute, xK_quotedbl, xK_apostrophe, xK_parenleft, xK_minus, xK_egrave, xK_underscore, xK_ccedilla]
        , (f, m) <- [(W.greedyView, 0), (W.shift, shiftMask)]]
    ++
    -- mod-[w,e] %! switch to twinview screen 1/2
    -- mod-shift-[w,e] %! move window to screen 1/2
    [((m .|. modMask, key), screenWorkspace sc >>= flip whenJust (windows . f))
        | (key, sc) <- zip [xK_a, xK_e] [0..]
        , (f, m) <- [(W.view, 0), (W.shift, shiftMask)]]

-------------------------------------------------------------------------------

