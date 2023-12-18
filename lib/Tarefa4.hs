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
                        Subir -> usaEscada p a
                        Descer -> usaEscada p a
                        AndarDireita -> moveDireita p a
                        AndarEsquerda -> moveEsquerda p a
                        Saltar -> salta p a
                        Parar -> para p a

usaEscada :: Personagem -> Acao -> Personagem
usaEscada p a = undefined

moveDireita :: Personagem -> Acao -> Personagem
moveDireita p a = undefined

moveEsquerda :: Personagem -> Acao -> Personagem
moveEsquerda p a = undefined

salta :: Personagem -> Acao -> Personagem
salta p a = undefined

para :: Personagem -> Acao -> Personagem
para p a = undefined