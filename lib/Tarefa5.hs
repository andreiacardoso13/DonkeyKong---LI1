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
import Mapa
-- import Main
import Tarefa1
import Tarefa2
import Tarefa4

import Data.Fixed

import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

data Estado = Estado {menu :: Menu, jogo :: Jogo, imagens :: Imagens, tempo :: Tempo, bonus :: Int, highScore :: [(Int,String)] }

data Menu = Inicio
          | Opcoes Opcao
          | ModoJogo
          | ModoPausa Opcao
          | ModoHighScore
          | ModoControlos
          | GanhouJogo
          | PerdeuJogo
          deriving Eq

data Opcao = Jogar
           | HighScore
           | Continuar
           | Reiniciar
           | Home
           | Controls
          deriving Eq

keys :: Event -> Estado -> Estado
keys evt s | menu s == Inicio = keysInicio evt s
           | menu s == Opcoes Jogar = keysOpJogar evt s
           | menu s == Opcoes HighScore = keysOpHighScore evt s
           | menu s == ModoJogo = keysModoJogo evt s
           | menu s == ModoPausa Continuar = keysModoPausa evt s
           | menu s == ModoPausa Reiniciar = keysModoPausa evt s
           | menu s == ModoPausa Controls = keysModoPausa evt s
           | menu s == ModoPausa Home = keysModoPausa evt s
           | menu s == ModoHighScore = keysModoHighScore evt s
           | menu s == GanhouJogo = keysGanhouJogo evt s
           | menu s == PerdeuJogo = keysPerdeuJogo evt s
           | menu s == ModoControlos = keysControlos evt s

keysInicio :: Event -> Estado -> Estado
keysInicio (EventKey (SpecialKey KeyEnter) Down _ _) s = s {menu = Opcoes Jogar}
keysInicio _ s = s

keysOpJogar :: Event -> Estado -> Estado
keysOpJogar (EventKey (SpecialKey KeyEnter) Down _ _) s = s {menu = ModoJogo, jogo = j1, tempo = 0, bonus = 15000}
keysOpJogar (EventKey (SpecialKey KeyDown) Down _ _) s = s {menu = Opcoes HighScore}
--keysOpJogar (EventKey (Char 'a') Down _ _)
keysOpJogar _ s = s

keysOpHighScore :: Event -> Estado -> Estado
keysOpHighScore (EventKey (SpecialKey KeyEnter) Down _ _) s = s {menu = ModoHighScore}
keysOpHighScore (EventKey (SpecialKey KeyUp) Down _ _) s = s {menu = Opcoes Jogar}
keysOpHighScore _ s = s






keysModoJogo :: Event -> Estado -> Estado
keysModoJogo (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc})})}) | not esc   = e{jogo = j {jogador = movePersonagem (jgd {posicao = (min x 27.5, y)}) (Just AndarDireita)}}
                                                                                                                          | otherwise = e

keysModoJogo (EventKey (SpecialKey KeyLeft)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                         jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                    emEscada = esc})})}) | not esc   = e{jogo = j {jogador = movePersonagem (jgd {posicao = (max x 0.5, y)}) (Just AndarEsquerda)}}
                                                                                                                         | otherwise = e

keysModoJogo (EventKey (SpecialKey KeyUp)    Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})})}) | b = e
                                                                                                                              |     esc && procuraBloco blocos pos == Vazio && procuraBlocoInf blocos pos == Plataforma && colisoesParede m jgd = e{jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}}
                                                                                                                              | not esc && procuraBloco blocos pos == Escada && mod' x 1 <= 0.6 && mod' x 1 >= 0.3                              = e{jogo = j {jogador = movePersonagem jgd (Just Subir)}}
                                                                                                                              | esc                                                                                                             = e{jogo = j {jogador = movePersonagem (jgd {posicao = (x, max (y-0.5) 0.5)}) (Just Subir)}}
                                                                                                                              | otherwise = e

keysModoJogo (EventKey (SpecialKey KeyDown)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})})}) | b = e
                                                                                                                              |    esc  && procuraBlocoInf blocos pos == Escada                                                                                    = e{jogo = j {jogador = movePersonagem (jgd {posicao = (x, min (y+0.5) 16.5)}) (Just Descer)}}
                                                                                                                              |    esc  && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos pos     == Escada && colisoesParede m jgd               = e{jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}}
                                                                                                                              |            procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos (x,y+2) == Escada && mod' x 1 <= 0.6 && mod' x 1 >= 0.3 = e{jogo = j {jogador = movePersonagem (jgd {posicao = (x, min (y+0.5) 16.5)}) (Just Descer)}}
                                                                                                                              |            procuraBlocoInf blocos pos == Escada     && procuraBloco blocos pos     == Plataforma                                   = e{jogo = j {jogador = movePersonagem jgd (Just Descer)}}

keysModoJogo (EventKey (SpecialKey KeyUp) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc})})}) | esc && procuraBloco blocos pos == Vazio && procuraBlocoInf blocos pos == Plataforma && colisoesParede m jgd = e{jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}}
                                                                                                                          | otherwise = e {jogo = j {jogador = movePersonagem jgd (Just Parar)}}


keysModoJogo (EventKey (SpecialKey k) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc})})}) = if k == KeyRight || k == KeyLeft || k == KeyUp || k == KeyDown
                                                                                                                               then e{jogo = j {jogador = movePersonagem jgd (Just Parar)}}
                                                                                                                               else e


keysModoJogo (EventKey (SpecialKey KeySpace) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})}),
                                                                                                     tempo = t})             | b = e
                                                                                                                             | not (colisoesParede m jgd)            = e{jogo = j {jogador = movePersonagem jgd Nothing}}
                                                                                                                             | not esc  && fst (velocidade jgd) == 0 = e{jogo = j {jogador = movePersonagem (jgd {posicao = (x, y-1)}) (Just Saltar)}}
                                                                                                                             | not esc  && direcao jgd == Oeste      = e{jogo = j {jogador = movePersonagem (jgd {posicao = (x-1, y-1)}) (Just Saltar)}}
                                                                                                                             | not esc  && direcao jgd == Este       = e{jogo = j {jogador = movePersonagem (jgd {posicao = (x+1, y-1)}) (Just Saltar)}}
                                                                                                                             | otherwise = e

keysModoJogo (EventKey (SpecialKey KeySpace) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                          jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                     emEscada = esc,
                                                                                                     aplicaDano = (b,_)})}),
                                                                                                     tempo = t})             | b = e
                                                                                                                             | otherwise = e{jogo = j {jogador = movePersonagem jgd Nothing}}


keysModoJogo (EventKey (Char 'p') Down _ _) e@(Estado {menu = ModoJogo}) = e {menu = ModoPausa Continuar}

keysModoJogo _ e = e


keysModoPausa :: Event -> Estado -> Estado
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Continuar}) = e {menu = ModoJogo}
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Reiniciar}) = e {menu = ModoJogo, jogo = j1, tempo = 0, bonus = 15000}
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Home})      = e {menu = Opcoes Jogar, jogo = jOpcoes}
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Controls})  = e {menu = ModoControlos}

keysModoPausa (EventKey (SpecialKey KeyUp) Down _ _)   e@(Estado {menu = ModoPausa Continuar}) = e {menu = ModoPausa Reiniciar}
keysModoPausa (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {menu = ModoPausa Home})      = e {menu = ModoPausa Continuar}
keysModoPausa (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {menu = ModoPausa Reiniciar}) = e {menu = ModoPausa Continuar}
keysModoPausa (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {menu = ModoPausa Controls})  = e {menu = ModoPausa Continuar}

keysModoPausa (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {menu = ModoPausa Reiniciar}) = e {menu = ModoPausa Home}
keysModoPausa (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {menu = ModoPausa Home})      = e {menu = ModoPausa Controls}
keysModoPausa (EventKey (SpecialKey KeyLeft) Down _ _)  e@(Estado {menu = ModoPausa Controls})  = e {menu = ModoPausa Home}
keysModoPausa (EventKey (SpecialKey KeyLeft) Down _ _)  e@(Estado {menu = ModoPausa Home})      = e {menu = ModoPausa Reiniciar}

keysModoPausa _ s = s


keysModoHighScore :: Event -> Estado -> Estado
keysModoHighScore (EventKey (SpecialKey KeyEnter) Down _ _) s = s {menu = Opcoes Jogar}
keysModoHighScore _ s = s


keysGanhouJogo :: Event -> Estado -> Estado
keysGanhouJogo (EventKey (Char 'a') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "A"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'b') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "B"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'c') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "C"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'd') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "D"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'e') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "E"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'f') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "F"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'g') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "G"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'h') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "H"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'i') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "I"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'j') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "J"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'k') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "K"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'l') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "L"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'm') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "M"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'n') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "N"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'o') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "O"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'p') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "P"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'q') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "Q"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'r') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "R"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 's') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "S"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 't') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "T"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'u') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "U"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'v') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "V"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'w') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "W"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'x') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "X"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'y') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "Y"}
                                                | otherwise = s
keysGanhouJogo (EventKey (Char 'z') Down _ _) s | length (snd (last (highScore s)))<= 8 = s {highScore = escreve (highScore s) "Z"}
                                                | otherwise = s
keysGanhouJogo (EventKey (SpecialKey KeyDelete) Down _ _) s | null (snd (last (highScore s))) = s
                                                            | otherwise = s {highScore = remove (highScore s)}
keysGanhouJogo (EventKey (SpecialKey KeyEnter) Down _ _) s = s {menu = Opcoes Jogar, jogo = jOpcoes}

keysGanhouJogo _ s = s

remove :: [(Int,String)] -> [(Int,String)]
remove [] = []
remove l = init l ++ [(fst (last l), init (snd (last l)))]

--função que adiciona uma letra ao highScore
escreve :: [(Int,String)] -> String -> [(Int,String)]
escreve l a = init l ++ [(fst (last l), snd (last l) ++ a)]


keysPerdeuJogo :: Event -> Estado -> Estado
keysPerdeuJogo _ s = s
--funcao que carrega no enter e volta para menu


-- EventKey Key KeyState Modifiers (Float, Float)

keysControlos :: Event -> Estado -> Estado
keysControlos (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoControlos}) = e {menu = ModoPausa Controls}
keysControlos _ s = s