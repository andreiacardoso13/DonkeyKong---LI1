{-|
Module      : Tarefa3
Description : Movimenta personagens no jogo
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 de LI1 em 2023/24.
-}
module Tarefa3 where

import LI12324
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta = undefined

{-|

Define a área onde um personagem consegue causar dano,
sendo esta no meu tamanho do menor retângulo que contém um personagem

=Exemplos
>>> hitboxDano (Personagem {posicao = (1,1), tamanho = (1,1), direcao = Este}) = ((1.5 , 0.5) , (2.5 , 1.5))
>>> hitboxDano (Personagem {posicao = (1,1), tamanho = (1,1), direcao = Oeste}) = ((-0.5 , 0.5) , (0.5 , 1.5))
-}

hitboxDano :: Personagem -> Hitbox
hitboxDano (Personagem {posicao = (x,y), tamanho = (l,a), direcao = dir}) | dir == Oeste = ((x-(3*l/2),y-(a/2)),(x - (l/2), y + (a/2)))
                                                                          | dir == Este = ((x + (l/2),y-(a/2)),(x + (3*l/2),y + (a/2)))




{-
Um inimigo perde 1 (uma) vida se estiver dentro da hitbox de dano de
um jogador armado. Por jogador armado entende-se um jogador cuja
componente aplicaDano esteja activa e com tempo restante. Note
que a hitbox de dano n˜ao ´e a mesma hitbox do jogador, mas antes uma
hitbox com as dimens˜oes do jogador posicionada exactamente `a frente
do jogador, cf. 

-}