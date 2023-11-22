{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324

hitbox :: Personagem -> Hitbox
hitbox (Personagem {posicao = (x,y), tamanho = (c,l)}) = (((x+(c/2)),(y+(c/2))),((x-(c/2)), y-((c/2))))



colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede = undefined

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens = undefined
