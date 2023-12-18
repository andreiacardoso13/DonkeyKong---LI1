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

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza = undefined

atualizaInimigos :: Mapa -> Personagem -> Personagem
atualizaInimigos m p = undefined

atualizaJogador :: Mapa -> Personagem -> Personagem
atualizaJogador m p = undefined

movePersonagem :: Personagem -> Acao -> Personagem
movePersonagem p a = case a of
                        Subir -> usaEscada p Subir
                        Descer -> usaEscada p Descer
                        AndarDireita -> moveDireita p AndarDireita
                        AndarEsquerda -> moveEsquerda p AndarEsquerda
                        Saltar -> salta p Saltar
                        Parar -> para p Parar

usaEscada :: Personagem -> Acao -> Personagem
usaEscada p a | a == Subir =  (p {velocidade = (0,-10), direcao = Norte})
              | a == Descer = (p {velocidade = (0, 10), direcao = Norte})

moveDireita :: Personagem -> Acao -> Personagem
moveDireita p AndarDireita = (p {velocidade = (10,0), direcao = Este})

moveEsquerda :: Personagem -> Acao -> Personagem
moveEsquerda p AndarEsquerda = (p {velocidade = (-10,0), direcao = Oeste})

salta :: Personagem -> Acao -> Personagem
salta p a = undefined

para :: Personagem -> Acao -> Personagem
para p Parar = (p {velocidade = (0,0)})