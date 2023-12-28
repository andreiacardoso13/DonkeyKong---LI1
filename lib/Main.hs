module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Imagens
import Mapa
import Graphics.Gloss.Juicy -- precisa apenas de ficar no Imagens ACHO


{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

data Estado = Estado {jogo :: Jogo, imagens :: Imagens}


main :: IO ()
main = do
  images <- getImages
  play janela
       bg
       fr
       (estadoInicial images)
       desenhaEstado
       reageEvento
       reageTempo



janela :: Display
janela = InWindow
  "Mapa"     -- título de janela    
  (500,500)  -- dimensão da janela
  (10,10)    -- posição no ecrã

bg :: Color
bg = white

fr :: Int
fr = 20

estadoInicial :: Imagens -> Estado
estadoInicial images = Estado {jogo = j1, imagens = images}

desenhaEstado :: Estado -> Picture
desenhaEstado s = Pictures[getImagem MarioStandingRight (imagens s), Translate 10 10 (getImagem Ladder (imagens s)), Translate (-30) 30 (Scale 0.2 0.2 (getImagem Coin (imagens s)))]


reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

reageTempo :: Float -> Estado -> Estado
reageTempo _ s = s


