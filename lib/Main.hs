module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324

{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

data Estado = Estado {posicao :: (Float,Float)
                     , vel :: (Float,Float)
                     }

get_images :: IO [Picture]
get_images = do
               escada              <- loadBMP "imagens/ladder.bmp"
               alcapao             <- loadBMP "imagens/alcapao.bmp"
               plataforma          <- loadBMP "imagens/plataforma.bmp"
               estrela             <- loadBMP "imagens/star.bmp"
               moeda               <- loadBMP "imagens/coin.bmp"
               vazio               <- loadBMP "imagens/Vazio.bmp"
               let images = [escada, alcapao, plataforma, estrela, moeda]
               return images


main :: IO ()
main = do 
        imagens <- get_images
        play  janela                          -- janela onde irá decorrer o jogo
              bg                              -- cor do fundo da janela
              fr                              -- frame rate
              estadoInicial                   -- define estado inicial do jogo
              desenhaEstado                   -- desenha o estado do jogo
              reageEvento                     -- reage a um evento
              reageTempo                      -- reage ao passar do tempo

janela :: Display
janela = InWindow
  "Mapa"     -- título de janela    
  (500,500)  -- dimensão da janela
  (10,10)    -- posição no ecrã

bg :: Color
bg = white

fr :: Int
fr = 20

estadoInicial :: Estado
estadoInicial = Estado (0,0) (50,75)

desenhaEstado :: Estado -> Picture
desenhaEstado e = rectangleSolid 50 50

reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

reageTempo :: Float -> Estado -> Estado
reageTempo t e = e