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
               escada              <- loadBMP "imagens/ladder.bmp"
               alcapao             <- loadBMP "imagens/alcapao.bmp"
               plataforma          <- loadBMP "imagens/plataforma.bmp"
               estrela             <- loadBMP "imagens/star.bmp"
               moeda               <- loadBMP "imagens/coin.bmp"
               let images = [escada, alcapao, plataforma, estrela, moeda]
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