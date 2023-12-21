{-|
Module      : Tarefa4
Description : Atualiza as velocidades das personagens no jogo
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 de LI1 em 2023/24.
-}
module Tarefa4 where

import Data.Maybe
import Tarefa1
import Tarefa2
import Tarefa3
import LI12324

{-| Recebe as ações a aplicar aos inimigos, a ação a aplicar ao jogador, e um jogo, devolvendo o jogo atualizado.

= Exemplos

>>> atualiza [Just Parar, Nothing] (Just AndarDireita) (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]],
                                                         inimigos = [Personagem {velocidade = (0,10),tipo = Fantasma,posicao = (1.5,1.5),direcao = Norte, tamanho = (1,1),emEscada = True,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)},
                                                                     Personagem {velocidade = (10,0),tipo = Fantasma,posicao = (0.5,1.5),direcao = Este, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}],
                                                         colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))],
                                                         jogador = Personagem {velocidade = (-10,0),tipo = Jogador,posicao = (2.5,1.5),direcao = Oeste, tamanho = (1,1),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
             = Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]],
                     inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Norte, tamanho = (1.0,1.0), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)},
                                 Personagem {velocidade = (10.0,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}],
                     colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))],
                     jogador = Personagem {velocidade = (10.0,0.0), tipo = Jogador, posicao = (2.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5.0)}}
-}

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza (ai:ais) aj j@(Jogo {inimigos = (i:is), jogador = jgd}) = (j {inimigos = atualizaInimigos (i:is) (ai:ais), jogador = movePersonagem jgd aj})

{-| Recebe a lista dos inimigos de um jogo e a lista das ações a aplicar-lhes, devolvendo a lista de inimigos com as respetivas direções e velocidades atualizadas.

= Exemplos

>>> atualizaInimigos [Personagem {velocidade = (-10,0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}, Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (3.5,5.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}] [Just AndarDireita, Just AndarEsquerda]
                     = [Personagem {velocidade = (10.0,0.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)},Personagem {velocidade = (-10.0,0.0), tipo = Fantasma, posicao = (3.5,5.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
>>> atualizaInimigos [Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (3,2), direcao = Este, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}] [Just Subir]
                     = [Personagem {velocidade = (0.0,-10.0), tipo = Fantasma, posicao = (3.0,2.0), direcao = Norte, tamanho = (1.0,1.0), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
-}

atualizaInimigos :: [Personagem] -> [Maybe Acao] -> [Personagem]
atualizaInimigos [] [] = []
atualizaInimigos (i:is) (ai:ais) =  movePersonagem i ai : atualizaInimigos is ais

{-| Recebe uma personagem e a ação a aplicar-lhe, devolvendo a personagem com a velocidade e direção atualizadas.

= Exemplos

>>> movePersonagem (Personagem {velocidade = (10,0), tipo = Fantasma, posicao = (3,2), direcao = Este, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) (Just Parar)
Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}

>>> movePersonagem (Personagem {velocidade = (10,0), tipo = Jogador, posicao = (3,2), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False,0)}) (Nothing)
Personagem {velocidade = (10.0,0.0), tipo = Fantasma, posicao = (3.0,2.0), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False,0.0)}
-}

movePersonagem :: Personagem -> Maybe Acao -> Personagem
movePersonagem p a = case a of
                        Just Subir -> usaEscada p Subir
                        Just Descer -> usaEscada p Descer
                        Just AndarDireita -> moveDireita p AndarDireita
                        Just AndarEsquerda -> moveEsquerda p AndarEsquerda
                        Just Saltar -> salta p Saltar
                        Just Parar -> para p Parar
                        Nothing -> p

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Subir' ou 'Descer'. -}

usaEscada :: Personagem -> Acao -> Personagem
usaEscada p a | a == Subir =  (p {velocidade = (0,-10), direcao = Norte})
              | a == Descer = (p {velocidade = (0, 10), direcao = Norte})

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'AndarDireita'. -}

moveDireita :: Personagem -> Acao -> Personagem
moveDireita p AndarDireita = (p {velocidade = (10,0), direcao = Este})

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'AndarEsquerda'. -}

moveEsquerda :: Personagem -> Acao -> Personagem
moveEsquerda p AndarEsquerda = (p {velocidade = (-10,0), direcao = Oeste})

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Saltar'. -}

salta :: Personagem -> Acao -> Personagem
salta p@(Personagem {velocidade = (h,v)}) Saltar = (p {velocidade = (h,-10)})

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Parar'. -}

para :: Personagem -> Acao -> Personagem
para p Parar = (p {velocidade = (0,0)})