module Main (main) where

import System.Exit
import XMonad
import XMonad.Config.Desktop
import XMonad.Hooks.DynamicLog
import XMonad.Hooks.ManageHelpers
import XMonad.Util.NamedScratchpad
import XMonad.Layout.BinarySpacePartition (emptyBSP)
import XMonad.Layout.NoBorders (noBorders)
import XMonad.Layout.ResizableTile (ResizableTall(..))
import XMonad.Layout.ToggleLayouts (ToggleLayout(..), toggleLayouts)
import XMonad.Layout.Spiral
import XMonad.Layout.Grid
import XMonad.Prompt
import XMonad.Prompt.ConfirmPrompt
import XMonad.Util.EZConfig
import XMonad.Util.Run (spawnPipe)
import XMonad.StackSet as W
import System.IO (hPutStrLn)
import XMonad.Prompt.Pass
import Data.Ratio ((%))

myTerminal :: [Char]
myTerminal = "kitty"

main :: IO ()
main = do
  xmproc <- spawnPipe "xmobar"
  xmonad $ desktopConfig
    { borderWidth = 1
    , focusedBorderColor = "#105010"
    , normalBorderColor  = "#000000"
    , modMask    = mod4Mask -- Use the "Win" key for the mod key
    , manageHook = myManageHook
               <+> manageHook desktopConfig
               <+> namedScratchpadManageHook scratchpads
    , layoutHook = desktopLayoutModifiers $ myLayouts
    , terminal   = myTerminal
    , logHook    = dynamicLogWithPP xmobarPP
        { ppOutput = hPutStrLn xmproc }
    , startupHook = do
        spawn "numlockx on"
        spawn "stalonetray"
        spawn "dunst"
        spawn "udiskie -q -N -a -t"
        spawn "nitrogen --restore"
    }
    `additionalKeysP` myKeys'
    `additionalKeys` myKeys

myKeys' :: [([Char], X ())]
myKeys' =
  [ ("M-S-q", confirmPrompt myXPConfig "exit" (io exitSuccess))
  -- , ("M-d"  , shellPrompt myXPConfig)
  , ("M-S-f", sendMessage (Toggle "Full"))
  , ("M-d", spawn "$(yeganesh -x)")
  -- , ("M-m", namedScratchpadAction scratchpads "poczta")
  , ("M-S-m", spawn "thunderbird")
  , ("M-m", spawn "firefox")
  -- , ("M-n", namedScratchpadAction scratchpads "odsluch")
  -- , ("M-S-g", namedScratchpadAction scratchpads "newsy")
  -- , ("M-S-b", namedScratchpadAction scratchpads "kalendarz")
  , ("M-b", namedScratchpadAction scratchpads "manager-plikow")
  ]

myKeys :: [((KeyMask, KeySym), X ())]
myKeys =
  [ ((mod4Mask, xK_p) , passPrompt myXPConfig)
  , ((mod4Mask .|. controlMask, xK_p), passGeneratePrompt myXPConfig)
  , ((mod4Mask .|. shiftMask, xK_z), spawn "xscreensaver-command -lock")
  , ((0, 0x1008FF11), spawn "amixer -q sset Master 2%-")
  , ((0, 0x1008FF13), spawn "amixer -q sset Master 2%+")
  , ((0, 0x1008FF12), spawn "amixer set Master toggle")
  ]

spawnTermApp :: String -> String -> String
spawnTermApp path className' = myTerminal
  ++ " --class " ++ className'
  ++ " --name " ++ className'
  ++ " -e " ++ path

newScratchpad :: String -> String -> NamedScratchpad
newScratchpad path className' =
  NS className' spawnS findS manageS
  where
    spawnS  = spawnTermApp path className'
    findS   = resource =? className'
    manageS = customFloating $ W.RationalRect l t w h
      where
        h = 0.9
        w = 0.9
        t = 0.95 - h
        l = 0.95 - w

scratchpads :: [NamedScratchpad]
scratchpads =
  [ newScratchpad "ncmpcpp"     "odsluch"
  , newScratchpad "newsboat"    "newsy"
  , newScratchpad "neomutt"     "poczta"
  , newScratchpad "calcurse"    "kalendarz"
  , newScratchpad "ranger"      "manager-plikow"
  ]

myLayouts = toggleLayouts (noBorders Full) others
  where
    others = ResizableTall 1 (1.5/100) (3/5) []
         ||| emptyBSP
         ||| spiral (125 % 146)
         ||| Grid
         ||| Mirror (Tall 1 (3/100) (3/5))

myXPConfig :: XPConfig
myXPConfig = def
  { position          = Top
  , alwaysHighlight   = True
  , promptBorderWidth = 0
  , font              = "-*-fixed-*-*-*-*-14-*-*-*-*-*-*-*"
  }

myManageHook :: ManageHook
myManageHook = composeOne
  [ className =? "Pidgin"  -?> doFloat
  , className =? "XCalc"   -?> doFloat
  , className =? "mpv"     -?> doFloat
  , className =? "mplayer" -?> doFloat
  , isDialog               -?> doCenterFloat

    -- Move transient windows to their parent:
  , transience
  ]
