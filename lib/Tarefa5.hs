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

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Estado = Estado {jogo :: Jogo, imagens :: Imagens, tempo :: Float} -- o tempo aumenta 4 por segundo, serve para alterar as imagens automaticamente

keys :: Event -> Estado -> Estado
keys (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) = e {jogo = j {jogador = jgd {posicao = newpos, 
                                                                                                                                                        emEscada = procuraBloco blocos newpos == Escada}}}
                                                                                                                            where newpos = (min (x+1) 27.5, y)

keys (EventKey (SpecialKey KeyLeft)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) = e {jogo = j {jogador = jgd {posicao = newpos, 
                                                                                                                                                        emEscada = procuraBloco blocos newpos == Escada}}}
                                                                                                                            where newpos = (max (x-1) 0.5, y)

keys (EventKey (SpecialKey KeyUp)    Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | esc && procuraBlocoSup blocos pos == Plataforma = e {jogo = j {jogador = jgd {posicao = newpos', 
                                                                                                                                                                                                          emEscada = procuraBloco blocos newpos' == Escada}}}
                                                                                                                          | esc                                             = e {jogo = j {jogador = jgd {posicao = newpos,
                                                                                                                                                                                                          emEscada = procuraBloco blocos newpos == Escada}}}
                                                                                                                          | otherwise = e
                                                                                                                                        where newpos  = (x, max (y-1) 0.5)
                                                                                                                                              newpos' = (x, max (y-2) 0.5)

keys (EventKey (SpecialKey KeyDown)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | esc && procuraBlocoInf blocos pos == Plataforma = e 
                                                                                                                          | esc && procuraBlocoInf blocos pos == Escada     = e {jogo = j {jogador = jgd {posicao = newpos, 
                                                                                                                                                                                                          emEscada = procuraBloco blocos newpos == Escada}}}
                                                                                                                                                                              where newpos = (x, min (y+1) 16.5)

keys (EventKey (SpecialKey KeySpace) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m, jogador = jgd@(Personagem {posicao = pos@(x,y)})})}) = e {jogo = j {jogador = jgd {posicao = (x, y+2)}}}
keys _ e = e

-- EventKey Key KeyState Modifiers (Float, Float)