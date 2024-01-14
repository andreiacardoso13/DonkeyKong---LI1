{-|
Module      : Music
Description : Comandos de controlo do ficheiro mp3
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a inclusão de música no projeto de LI1 em 2023/24.
-}

module Music where

import System.Process

-- | Executa o comando para iniciar a reprodução de música de fundo.
musicaMenu :: IO ProcessHandle
musicaMenu = spawnCommand "mpv --no-video --loop music/Title-BGM.mp3"

-- | Executa o comando para parar a reprodução de música.
musicaParar :: IO ProcessHandle
musicaParar = spawnCommand "killall mpv"
