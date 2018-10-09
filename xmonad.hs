
import Data.Tree
import Data.List (sortBy)
import Data.Function (on)
import System.Process
import Control.Monad (forM_, join)
import Control.Monad.IO.Class
import XMonad
import qualified Data.Map as M
import XMonad.Layout.Drawer
import XMonad.Hooks.ManageDocks
import XMonad.Hooks.EwmhDesktops (ewmh)
import XMonad.Layout.NoBorders
import XMonad.Layout.BinarySpacePartition
-- import System.Taffybar.Support.PagerHints (pagerHints)
import XMonad.Hooks.FadeInactive
import XMonad.Actions.CycleWindows
import XMonad.Util.EZConfig
import qualified XMonad.Layout.IndependentScreens as LIS
import XMonad.Actions.WindowBringer
import XMonad.Layout.Spacing
import XMonad.Layout.Minimize

import Data.Ratio
import XMonad.Util.Run
import XMonad.Actions.Warp
import XMonad.Actions.CycleWS
import XMonad.Layout.MultiColumns
import XMonad.Layout.Reflect
import XMonad.Util.NamedWindows (getName)
import qualified XMonad.StackSet as W

import XMonad.Actions.TreeSelect

windowCenter = warpToWindow (1 % 6) (1 % 6)

onStartup :: X ()
onStartup = do
  liftIO $ spawnProcess "nitrogen" ["--restore"]
  forM_ ["/tmp/.xmonad-workspace-log", "/tmp/.xmonad-title-log"] $ \file ->
    safeSpawn "mkfifo" [file]
  return ()

main :: IO ()
main = do
  --spawn "setxkbmap -option ctrl:nocaps"
  xmonad $ docks $ ewmh $ def
    { terminal = "termite" -- "urxvt"
    , normalBorderColor = "#000000"
    , focusedBorderColor = "#cd8b00"
    , borderWidth = 1
    , focusFollowsMouse = True
--    , logHook = fadeInactiveLogHook 0.5
    , modMask = mod4Mask
    , startupHook = onStartup
    , layoutHook = smartBorders myLayout
    , logHook = eventLogHook
    , workspaces = toWorkspaces myWorkspaces
    } `additionalKeys` keybindings


keybindings =
   [((mod4Mask,  xK_s), cycleRecentWindows [xK_Super_L] xK_s xK_w)
   , ((mod4Mask, xK_z), rotOpposite) , ((mod4Mask                , xK_i), rotUnfocusedUp)
   , ((mod4Mask                , xK_u), rotUnfocusedDown)
   , ((mod4Mask .|. controlMask, xK_i), rotFocusedUp)
   , ((mod4Mask .|. controlMask, xK_u), rotFocusedDown)

   , ((mod4Mask                , xK_g     ), gotoMenu)
   , ((mod4Mask                , xK_b     ), bringMenu)

   
   , ((mod4Mask,                 xK_semicolon), sendMessage Expand)


   , ((mod4Mask .|. controlMask, xK_Left       ), prevScreen >> windowCenter)
   , ((mod4Mask .|. controlMask, xK_Right      ), nextScreen >> windowCenter)
   , ((mod4Mask .|. controlMask, xK_Down       ), shiftPrevScreen)
   , ((mod4Mask .|. controlMask, xK_Up         ), shiftNextScreen)
   , ((mod4Mask .|. controlMask .|. shiftMask, xK_Down       ), shiftPrevScreen >> prevScreen >> windowCenter)
   , ((mod4Mask .|. controlMask .|. shiftMask, xK_Up         ), shiftNextScreen >> nextScreen >> windowCenter)


   , ((mod4Mask .|. mod1Mask,               xK_l     ), sendMessage $ ExpandTowards R)
   , ((mod4Mask .|. mod1Mask,               xK_semicolon     ), sendMessage $ ExpandTowards R)
   , ((mod4Mask .|. mod1Mask,               xK_h     ), sendMessage $ ExpandTowards L)
   , ((mod4Mask .|. mod1Mask,               xK_j     ), sendMessage $ ExpandTowards D)
   , ((mod4Mask .|. mod1Mask,               xK_k     ), sendMessage $ ExpandTowards U)
   , ((mod4Mask .|. mod1Mask .|. controlMask , xK_l     ), sendMessage $ ShrinkFrom R)
   , ((mod4Mask .|. mod1Mask .|. controlMask , xK_semicolon     ), sendMessage $ ShrinkFrom R)
   , ((mod4Mask .|. mod1Mask .|. controlMask , xK_h     ), sendMessage $ ShrinkFrom L)
   , ((mod4Mask .|. mod1Mask .|. controlMask , xK_j     ), sendMessage $ ShrinkFrom D)
   , ((mod4Mask .|. mod1Mask .|. controlMask , xK_k     ), sendMessage $ ShrinkFrom U)

   , ((mod4Mask,                           xK_r     ), sendMessage RotateL)
   , ((mod4Mask,                           xK_d     ), sendMessage Swap)
   , ((mod4Mask,                           xK_n     ), sendMessage FocusParent)
   , ((mod4Mask .|. controlMask,           xK_n     ), sendMessage SelectNode)
   , ((mod4Mask .|. shiftMask,             xK_n     ), sendMessage MoveNode)

   , ((mod4Mask,               xK_a),     sendMessage Balance)
   , ((mod4Mask .|. shiftMask, xK_a),     sendMessage Equalize)

   , ((mod4Mask, xK_f), treeselectWorkspace tsDefaultConfig myWorkspaces W.greedyView)
   , ((mod4Mask .|. shiftMask, xK_f), treeselectWorkspace tsDefaultConfig myWorkspaces W.shift)
   ]

myWorkspaces :: Forest String
myWorkspaces = [ Node "Browser" []
               , Node "Home"
                   [ Node "1" []
                   , Node "2" []
                   , Node "3" []
                   , Node "4" []
                   ]
               , Node "ZL"
                   [ Node "Lobby" []
                   , Node "VEDriver" []
                   ]
               , Node "Background"
                   [ Node "Spotify" []
                   ]
               ]
-- togglevga = do
--   screencount <- LIS.countScreens
--   if screencount > 1
--    then spawn "xrandr --output VGA-2 --off"
--    else spawn "xrandr --output VGA-1 --auto --left-of VGA-2"

myLayout = (spacing 6 $ layouts) ||| Full
  where
    layouts = avoidStruts (
      emptyBSP |||
      tiled ||| 
      -- myMultiCols ||| 
      --drawer `onLeft` tiled |||
      --drawer `onLeft` myMultiCols |||
      Mirror tiled)
    mumble = ClassName "Mumble" `Or` ClassName "mumble"
    telegram = Title "Telegram" `And` ClassName "TelegramDesktop"
    keepassx = ClassName "keepassx" `Or` ClassName "Keepassx"
    mattermost = ClassName "mattermost" `Or` ClassName "Mattermost"

    myMultiCol = multiCol [1] 1 0.05 0.4
    myMultiCols = reflectHoriz myMultiCol ||| myMultiCol

    drawer = simpleDrawer 0.01 0.2 (mumble `Or` telegram `Or` keepassx `Or` mattermost)
    -- default tiling algorithm partitions the screen into two panes
    tiled   = Tall nmaster delta ratio

    -- The default number of windows in the master pane
    nmaster = 1

    -- Default proportion of screen occupied by master pane
    ratio   = 1/2

    -- Percent of screen to increment by when resizing panes
    delta   = 3/100

eventLogHook = do
  winset <- gets windowset
  title <- maybe (return "") (fmap show . getName) . W.peek $ winset
  let currWs = W.currentTag winset
  let wss = map W.tag $ W.workspaces winset
  let wsStr = join $ map (fmt currWs) $ sort' wss

  io $ appendFile "/tmp/.xmonad-title-log" (title ++ "\n")
  io $ appendFile "/tmp/.xmonad-workspace-log" (currWs ++ "\n")

  where fmt currWs ws
          | currWs == ws = "[" ++ ws ++ "]"
          | otherwise    = " " ++ ws ++ " "
        sort' = sortBy (compare `on` (!! 0))
