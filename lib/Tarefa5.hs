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
import Tarefa4

import Data.Fixed

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Estado = Estado {jogo :: Jogo, imagens :: Imagens, tempo :: Float} -- o tempo aumenta 4 por segundo, serve para alterar as imagens automaticamente

keys :: Event -> Estado -> Estado
keys (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | not esc = e {jogo = j {jogador = movePersonagem (jgd {posicao = (min (x+0.5) 27.5, y)}) (Just AndarDireita)}}
                                                                                                                          | otherwise = e

keys (EventKey (SpecialKey KeyLeft) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                         jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                    emEscada = esc})})}) | not esc = e {jogo = j {jogador = movePersonagem (jgd {posicao = (max (x-0.5) 0.5, y)}) (Just AndarEsquerda)}}
                                                                                                                         | otherwise = e

keys (EventKey (SpecialKey KeyUp)    Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | not esc && procuraBloco blocos pos == Escada && mod' x 1 /= 0 = e {jogo = j {jogador = movePersonagem jgd (Just Subir)}}
                                                                                                                          | esc     && procuraBloco blocos pos == Vazio  && mod' y 1 /= 0  = e {jogo = j {jogador = movePersonagem (jgd {posicao = (x, max (y-0.5) 0.5)}) (Just Parar)}}
                                                                                                                          | esc                                                           = e {jogo = j {jogador = jgd {posicao = (x, max (y-0.5) 0.5)}}}
                                                                                                                          | otherwise = e

keys (EventKey (SpecialKey KeyDown)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | not esc && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos (x,y+2) == Escada && mod' x 1 /= 0 = e {jogo = j {jogador = movePersonagem (jgd) (Just Descer)}}
                                                                                                                          | esc && procuraBlocoInf blocos pos == Plataforma = e 
                                                                                                                          | esc && procuraBlocoInf blocos pos == Escada     = e {jogo = j {jogador = jgd {posicao = (x, min (y+1) 16.5)}}}

keys (EventKey (SpecialKey KeySpace) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = pos@(x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (x, y+2)}}}

keys (EventKey (SpecialKey KeyRight) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) = e {jogo = j {jogador = movePersonagem jgd (Just Parar)}}

keys (EventKey (SpecialKey KeyLeft) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) = e {jogo = j {jogador = movePersonagem jgd (Just Parar)}}


keys _ e = e

-- EventKey Key KeyState Modifiers (Float, Float)