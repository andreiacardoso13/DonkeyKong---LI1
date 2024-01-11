module Music where

import System.Process

musicaMenu :: IO ProcessHandle
musicaMenu = spawnCommand "mpv --no-video --loop music/Title-BGM.mp3"

musicaParar :: IO ProcessHandle
musicaParar = spawnCommand "killall mpv"
