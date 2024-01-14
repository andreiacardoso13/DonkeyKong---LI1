{-|
Module      : Tarefa4_202324li1g005_Spec
Description : Testes da Tarefa 4
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização dos testes da Tarefa 4 de LI1 em 2023/24.
-}

module Tarefa4_202324li1g005_Spec where

import Tarefa4
import LI12324
import Test.HUnit

-- | Exemplo de jogo para testes
j = Jogo {mapa = mapa1
         ,inimigos = [en1, en2, en3]
         ,colecionaveis = []
         ,jogador = jgd 
         }


-- | Exemplo de mapa
mapa1 = Mapa ((0.5,2.5),Oeste) (1.5,0.5) [[Vazio     ,Vazio     ,Vazio     ],
                                          [Vazio     ,Plataforma,Vazio     ],
                                          [Vazio     ,Escada    ,Vazio     ],
                                          [Plataforma,Plataforma,Plataforma]]

-- | Exemplo de inimigo
en1 = Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1.5,2.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de inimigo
en2 = Personagem {velocidade = (1.5,0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de inimigo
en3 = Personagem {velocidade = (1.5,0), tipo = Fantasma, posicao = (2.5,2.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de jogador
jgd = Personagem {velocidade = (-5,0), tipo = Jogador, posicao = (0.5,2.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 2, pontos = 0, aplicaDano = (False,0)}


teste1 = "T1: Atualiza as direções e velocidades de todas as personagens" ~:  j {inimigos = [en1 {velocidade = (0,-5), direcao = Norte, emEscada = True}, en2, en3 {velocidade = (0,0)}], jogador = jgd {velocidade = (5,0), direcao = Este}} ~=? atualiza [Just Subir, Nothing, Just Parar] (Just AndarDireita) j

teste2 = "T2: Atualiza as direções e velocidades dos inimigos" ~: [en1 {velocidade = (0,-5), direcao = Norte, emEscada = True}, en2 {velocidade = (0.0,0.0)}] ~=? atualizaInimigos mapa1 [en1, en2] [Just Subir, Just Parar]

teste3 = "T3: Atualiza a direção e velocidade do jogador" ~: jgd ~=? atualizaPersonagem mapa1 jgd Nothing

testesTarefa4 = test [teste1,teste2,teste3]
