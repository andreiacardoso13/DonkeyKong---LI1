module Main where

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324

{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

get_images :: IO [Picture]
get_images = do
               Escada              <- loadBMP "imagens/ladder.bmp"
               Alcapao             <- loadBMP "imagens/alcapao.bmp"
               Plataforma          <- loadBMP "imagens/plataforma.bmp"
               Estrela             <- loadBMP "imagens/star.bmp"
               Moeda               <- loadBMP "imagens/coin.bmp"
               let images = [Escada, Alcapao, Plataforma, Estrela, Moeda]
               return images

main :: IO ()
main = do 
        imagens <- get_images
        play  janela                          -- janela onde irá decorrer o jogo
              bg                              -- cor do fundo da janela
              fr                              -- frame rate
              estadoinicial                   -- define estado inicial do jogo
              desenhaEstado                   -- desenha o estado do jogo
              reageEvento                     -- reage a um evento
              reageTempo                      -- reage ao passar do tempo

janela :: Display
janela = InWindow
  "Mapa"     -- título de janela    
  (500,500)  -- dimensão da janela
  (10,10)    -- posição no ecrã