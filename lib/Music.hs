module Music where

import System.Process

-- | Executa o comando para iniciar a reprodução de música de fundo.
musicaMenu :: IO ProcessHandle
musicaMenu = spawnCommand "mpv --no-video --loop music/Title-BGM.mp3"

-- | Executa o comando para parar a reprodução de música.
musicaParar :: IO ProcessHandle
musicaParar = spawnCommand "killall mpv"
