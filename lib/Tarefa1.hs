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
hitbox (Personagem {posicao = (x,y), tamanho = (c,l)}) = ((x-(c/2),y-(l/2)),(x+(c/2), y+(l/2)))

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede (Mapa (_,_) _ _) p = undefined 
-- true if the 1st htbx coord is on the left border or the 2nd htbx coord is on the right border 

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1@(Personagem {posicao = (x1,y1), tamanho = (c1,l1)}) p2@(Personagem {posicao = (x2,y2), tamanho = (c2,l2)}) = compHtbx hb1 hb2 || compHtbx hb2 hb1
    where hb1 = hitbox p1
          hb2 = hitbox p2
          compHtbx hb1 hb2 = fst(fst hb1) >= fst(fst hb2) && fst(fst hb1) <= fst(snd hb2) && snd(fst hb1) >= snd(fst hb2) && snd(fst hb1) <= snd(snd hb2)
