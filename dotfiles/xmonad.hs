import XMonad

main = xmonad defaultConfig
        { terminal    = "termite"
        , modMask     = mod4Mask
        , borderWidth = 10
        }
