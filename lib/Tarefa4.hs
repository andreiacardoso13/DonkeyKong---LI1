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

atualizaInimigos :: Personagem -> Maybe Acao -> Personagem
atualizaInimigos m p = undefined

atualizaJogador :: Personagem -> Maybe Acao -> Personagem
atualizaJogador m p = undefined

movePersonagem :: Personagem -> Maybe Acao -> Personagem
movePersonagem p a = case a of
                        Just Subir -> usaEscada p Subir
                        Just Descer -> usaEscada p Descer
                        Just AndarDireita -> moveDireita p AndarDireita
                        Just AndarEsquerda -> moveEsquerda p AndarEsquerda
                        Just Saltar -> salta p Saltar
                        Just Parar -> para p Parar

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