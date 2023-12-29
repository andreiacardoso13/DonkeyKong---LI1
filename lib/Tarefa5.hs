{-|
Module      : Tarefa5
Description : 
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 de LI1 em 2023/24.
-}
module Tarefa5 where

import LI12324
import Imagens
-- import Main
import Tarefa1
import Tarefa2

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Estado = Estado {jogo :: Jogo, imagens :: Imagens}

keys :: Event -> Estado -> Estado
keys (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = (x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (min (x+1) 27.5, y)}}}
keys (EventKey (SpecialKey KeyLeft)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = (x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (max (x-1) 0.5, y)}}}
keys (EventKey (SpecialKey KeyUp)    Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = (x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (x, min (y+1) 17.5)}}}
keys (EventKey (SpecialKey KeyDown)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = (x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (x, max (y-1) 0.5)}}}
keys (EventKey (SpecialKey KeySpace) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = (x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (x, y+2)}}}
keys _ e = e

-- EventKey Key KeyState Modifiers (Float, Float)