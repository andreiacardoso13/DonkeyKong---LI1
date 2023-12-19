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

>>>
>>>
-}

atualiza :: [Maybe Acao] -> Maybe Acao -> Jogo -> Jogo
atualiza (ai:ais) aj j@(Jogo {inimigos = (i:is), jogador = jgd}) = (j {inimigos = atualizaInimigos (i:is) (ai:ais), jogador = movePersonagem jgd aj})

{-| Recebe a lista dos inimigos de um jogo e a lista das ações a aplicar-lhes, devolvendo a lista de inimigos com as respetivas direções e velocidades atualizadas.

= Exemplos

>>>
>>>
-}

atualizaInimigos :: [Personagem] -> [Maybe Acao] -> [Personagem]
atualizaInimigos [] [] = []
atualizaInimigos (i:is) (ai:ais) =  movePersonagem i ai : atualizaInimigos is ais

{-| Recebe as ações a aplicar aos inimigos, a ação a aplicar ao jogador, e um jogo, devolvendo o jogo atualizado.

= Exemplos

>>>
>>>
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

{-| Atualiza a velocidade e direção de uma personagem ao aplicar a ação 'Subir' ou 'Descer'. 

= Exemplos

>>>
>>>
-}

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