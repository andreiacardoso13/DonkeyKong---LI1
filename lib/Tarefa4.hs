{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}

module Tarefa4 where
import LI12324
import Tarefa1
import Tarefa2
import Data.Maybe
import Data.Fixed

{-| Recebe as ações a aplicar aos inimigos, a ação a aplicar ao jogador, e um jogo, devolvendo o jogo atualizado.

= Exemplo

>>> atualiza [Just Parar, Nothing] (Just AndarDireita) (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]],inimigos = [Personagem {velocidade = (0,5), tipo = Fantasma, posicao = (1.5,1.5), direcao = Norte, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}, Personagem {velocidade = (5,0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}], colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))],jogador = Personagem {velocidade = (-5,0), tipo = Jogador, posicao = (2.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
   = Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]],inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Norte, tamanho = (1.0,1.0), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}, Personagem {velocidade = (5.0,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))],jogador = Personagem {velocidade = (5.0,0.0), tipo = Jogador, posicao = (2.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5.0)}}
-}
atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza [] _ j = j
atualiza (ai:ais) aj j@(Jogo {mapa = m@(Mapa _ _ blocos),inimigos = (i:is), jogador = jgd}) = (j {inimigos = atualizaInimigos m (i:is) (ai:ais), jogador = atualizaPersonagem m jgd aj})

{-| Recebe a lista dos inimigos de um jogo e a lista das ações a aplicar-lhes, devolvendo a lista de inimigos com as respetivas direções e velocidades atualizadas.

= Exemplo

>>> atualizaInimigos (Mapa ((0.5,2.5),Oeste) (1.5,0.5) [[Vazio,Vazio,Vazio],[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]]) [Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1.5,2.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}, Personagem {velocidade = (1.5,0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}] [Just Subir, Just Parar]
   = [Personagem {velocidade = (0,-5), tipo = Fantasma, posicao = (1.5,2.5), direcao = Norte, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}, Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
-}
atualizaInimigos :: Mapa -> [Personagem] -> [Maybe Acao] -> [Personagem]
atualizaInimigos m [] [] = []
atualizaInimigos m (i:is) (ai:ais) =  atualizaPersonagem m i ai : atualizaInimigos m is ais

{-| Recebe uma personagem e a ação a aplicar-lhe, devolvendo a personagem com a velocidade e direção atualizadas.

= Exemplo

>>> atualizaPersonagem (Mapa ((0.5,2.5),Oeste) (1.5,0.5) [[Vazio,Vazio,Vazio],[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (1.5,0), tipo = Fantasma, posicao = (2.5,2.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) (Just Parar)
   = Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (2.5,2.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

-}
atualizaPersonagem :: Mapa -> Personagem -> Maybe Acao -> Personagem
atualizaPersonagem m p a = case a of
                               Just Subir -> sobe m p Subir
                               Just Descer -> desce m p Descer
                               Just AndarDireita -> moveDireita m p AndarDireita
                               Just AndarEsquerda -> moveEsquerda m p AndarEsquerda
                               Just Saltar -> salta m p Saltar
                               Just Parar -> para m p Parar
                               Nothing -> p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Subir' -}

sobe :: Mapa -> Personagem -> Acao -> Personagem
sobe m@(Mapa _ _ blocos) p@(Personagem {velocidade = (vx,vy),posicao = pos@(x,y),emEscada = esc,aplicaDano = (b,_)}) Subir 
   | b = p
   |     esc && procuraBloco blocos pos == Vazio  && procuraBlocoInf blocos pos == Plataforma && colisoesParede m p = p {velocidade = (0,0), direcao = Norte,emEscada = False}
   | not esc && procuraBloco blocos pos == Escada && mod' x 1 <= 0.6 && mod' x 1 >= 0.3                             = p {velocidade = (0,-5),direcao = Norte,emEscada = True}
   | esc                                                                                                            = p {velocidade = (0,-5),direcao = Norte,emEscada = True}
   | otherwise = p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Descer' -}

desce :: Mapa -> Personagem -> Acao -> Personagem
desce m@(Mapa _ _ blocos) p@(Personagem {velocidade = (vx,vy),posicao = pos@(x,y),emEscada = esc,aplicaDano = (b,_)}) Descer
   | b = p
   |    esc  && procuraBlocoInf blocos pos == Escada                                                                                    = p {velocidade = (0,5), emEscada = True}
   |    esc  && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos pos     == Escada && colisoesParede m p                 = p {velocidade = (0,0), emEscada = False}
   |            procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos (x,y+2) == Escada && mod' x 1 <= 0.6 && mod' x 1 >= 0.3 = p {velocidade = (0,5), emEscada = True}
   |            procuraBlocoInf blocos pos == Escada     && procuraBloco blocos pos     == Plataforma                                   = p {velocidade = (0,5), emEscada = True}
   | otherwise = p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'AndarDireita' -}

moveDireita :: Mapa -> Personagem -> Acao -> Personagem
moveDireita m@(Mapa _ _ blocos) p@(Personagem {velocidade = (vx,vy),posicao = pos@(x,y),emEscada = esc,aplicaDano = (b,_)}) AndarDireita
   | not esc && procuraBlocoInf blocos pos == Plataforma = (p {velocidade = (5,0), direcao = Este})
   | otherwise = p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'AndarEsquerda' -}

moveEsquerda :: Mapa -> Personagem -> Acao -> Personagem
moveEsquerda m@(Mapa _ _ blocos) p@(Personagem {velocidade = (vx,vy),posicao = pos@(x,y),emEscada = esc,aplicaDano = (b,_)}) AndarEsquerda
   | not esc && procuraBlocoInf blocos pos == Plataforma = (p {velocidade = (-5,0), direcao = Oeste})
   | otherwise = p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Saltar' -}

salta :: Mapa -> Personagem -> Acao -> Personagem
salta m@(Mapa _ _ blocos) p@(Personagem {velocidade = (vx,vy),posicao = (x,y),emEscada = esc,aplicaDano = (b,_)}) Saltar 
   | b || not (colisoesParede m p) = p
   | not esc && vx == 0 = p {velocidade = (0,-5),posicao = (x,y-1)}
   | not esc && direcao p == Oeste = p {velocidade = (vx,-5),posicao = (x-1, y-1)}
   | not esc && direcao p == Este  = p {velocidade = (vx,-5),posicao = (x+1, y-1)}
   | otherwise = p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Parar' -}

para :: Mapa -> Personagem -> Acao -> Personagem
para m p Parar = (p {velocidade = (0,0)})
