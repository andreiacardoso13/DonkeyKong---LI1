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

data Estado = Estado {menu :: Menu, jogo :: Jogo, imagens :: Imagens, tempo :: Tempo, bonus :: Int} 

data Menu = Opcoes Opcao
          | ModoJogo
          | GanhouJogo
          | PerdeuJogo
          deriving Eq

data Opcao = Jogar 
          deriving Eq

keys :: Event -> Estado -> Estado
keys (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | not esc = freefall (e {jogo = j {jogador = movePersonagem (jgd {posicao = (min (x+0.5) 27.5, y)}) (Just AndarDireita)}})
                                                                                                                          | otherwise = e

keys (EventKey (SpecialKey KeyLeft)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                         jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                    emEscada = esc})})}) | not esc = freefall (e {jogo = j {jogador = movePersonagem (jgd {posicao = (max (x-0.5) 0.5, y)}) (Just AndarEsquerda)}})
                                                                                                                         | otherwise = e

keys (EventKey (SpecialKey KeyUp)    Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})})}) | b = e
                                                                                                                              |     esc && procuraBloco blocos pos == Vazio && procuraBlocoInf blocos pos == Plataforma && colisoesParede m jgd = freefall (e {jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}})
                                                                                                                              | not esc && procuraBloco blocos pos == Escada && mod' x 1 /= 0                                                   = freefall (e {jogo = j {jogador = movePersonagem jgd (Just Subir)}})
                                                                                                                              | esc                                                                                                             = freefall (e {jogo = j {jogador = movePersonagem (jgd {posicao = (x, max (y-0.5) 0.5)}) (Just Subir)}})
                                                                                                                              | otherwise = e

keys (EventKey (SpecialKey KeyDown)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})})}) | b = e
                                                                                                                              |    esc  && procuraBlocoInf blocos pos == Escada                                                                       = freefall (e {jogo = j {jogador = movePersonagem (jgd {posicao = (x, min (y+0.5) 16.5)}) (Just Descer)}})
                                                                                                                              |    esc  && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos pos     == Escada && colisoesParede m jgd  = freefall (e {jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}})
                                                                                                                              |            procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos (x,y+2) == Escada && mod' x 1 /= 0         = freefall (e {jogo = j {jogador = movePersonagem (jgd {posicao = (x, min (y+0.5) 16.5)}) (Just Descer)}})
                                                                                                                              |            procuraBlocoInf blocos pos == Escada     && procuraBloco blocos pos     == Plataforma                      = freefall (e {jogo = j {jogador = movePersonagem jgd (Just Descer)}})

keys (EventKey (SpecialKey KeyUp) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) | esc && procuraBloco blocos pos == Vazio && procuraBlocoInf blocos pos == Plataforma && colisoesParede m jgd = freefall (e {jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}})
                                                                                                                          | otherwise = e {jogo = j {jogador = movePersonagem jgd (Just Parar)}}


keys (EventKey (SpecialKey k) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y), 
                                                                                                     emEscada = esc})})}) = if k == KeyRight || k == KeyLeft || k == KeyUp || k == KeyDown
                                                                                                                               then freefall (e {jogo = j {jogador = movePersonagem jgd (Just Parar)}})
                                                                                                                               else e


keys (EventKey (SpecialKey KeySpace) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})}),
                                                          tempo = t})                                                        | b = e
                                                                                                                             | not esc   = freefall (e {jogo = j {jogador = movePersonagem(jgd {posicao = (x, y-0.5)}) (Just Saltar)}})
                                                                                                                             | otherwise = e


keys _ e = e

-- EventKey Key KeyState Modifiers (Float, Float)


{-| Determina se um personagem está em queda livre.

-}
freefall :: Estado -> Estado
freefall e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos), 
                                    jogador = jgd@(Personagem {posicao = pos@(x,y)})})}) = if procuraBlocoInf blocos pos == Vazio
                                                                                              then freefall e {jogo = j {jogador = jgd {posicao = (x,y+0.5)}}} 
                                                                                              else e