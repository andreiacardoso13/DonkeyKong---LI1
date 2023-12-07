{-|
Module      : Tarefa1
Description : Verifica colisões
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 de LI1 em 2023/24.
-}
module Tarefa1 where

import LI12324
import Mapa

-- Personagem {velocidade = _, tipo = _, posicao = (x,y), direcao = _, tamanho = (c,l), emEscada = _, ressalta = _, vida = _, pontos = _, aplicaDano = _}
-- Personagem {velocidade = (2,2), tipo = Fantasma, posicao = (2.5,1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

{-| Define o menor retângulo que contém uma personagem ou objeto.

= Exemplos

>>> hitbox (Personagem {velocidade = (2,2), tipo = Fantasma, posicao = (2.5,1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = ((2.0,0.5),(3.0,1.5))
>>> hitbox (Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (1,5.5), direcao = Sul, tamanho = (2,3), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (False,0)}) = ((0.0,4.0),(2.0,7.0))
-}

hitbox :: Personagem -> Hitbox
hitbox (Personagem {velocidade = _, tipo = _, posicao = (x,y), direcao = _, tamanho = (c,l), emEscada = _, ressalta = _, vida = _, pontos = _, aplicaDano = _}) = ((x-(c/2),y-(l/2)),(x+(c/2), y+(l/2)))

{-| Testa se uma personagem se encontra em colisão com os limites do mapa.

= Exemplos

>>> mapaLimites mapaPrincipal (Personagem {velocidade = (-2,0), tipo = Fantasma, posicao = (0,0), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = True
>>> mapaLimites mapaPrincipal (Personagem {velocidade = (2,0), tipo = Fantasma, posicao = (4,3), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = False
-}

mapaLimites :: Mapa -> Personagem -> Bool
mapaLimites (Mapa _ _ blocos) p@(Personagem {velocidade = v, tipo = ent, posicao = (x,y), direcao = dir, tamanho = (c,l), emEscada = esc, ressalta = res, vida = vi, pontos = pts, aplicaDano = apdn}) = eLim <= 0 || dLim >= fromIntegral mapaLargura || bLim <= 0 || tLim >= fromIntegral mapaAltura
  where (eLim, bLim) = fst (hitbox p)
        (dLim, tLim) = snd (hitbox p)
        mapaLargura = length (head blocos)
        mapaAltura = length blocos
  -- "fromIntegral" faz os valores mapaLargura e mapaAltura serem doubles como os valores das coords

{- mapaChao :: Mapa -> Personagem -> Bool
mapaChao (Mapa _ _ blocos) p@(Personagem {posicao = (x,y)}) = blc == Plataforma && snd (fst (hitbox p)) == snd (snd (hitbox blc))
      where blc = procuraBlocoInf blocos (x,y) -}
-- hitbox is only defined for characters, not blocks!! 

procuraBlocoInf :: [[Bloco]] -> Posicao -> Bloco
procuraBlocoInf ((h:t):ls) (x,y) | y > 0 = procuraBlocoInf ls (x,y-1)
                                 | x > 0 = procuraBlocoInf [t] (x-1,y)
                                 | otherwise = h

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m@(Mapa _ _ blocos) p@(Personagem {posicao = (x,y)}) = mapaLimites m p -- || mapaChao m p
-- true if the 1st htbx coord is on the left border or the 2nd htbx coord is on the right border 

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = compHtbx hb1 hb2 || compHtbx hb2 hb1
    where hb1 = hitbox p1
          hb2 = hitbox p2
          compHtbx hb1 hb2 = fst (fst hb1) >= fst (fst hb2) && fst (fst hb1) <= fst (snd hb2) && snd (fst hb1) >= snd (fst hb2) && snd (fst hb1) <= snd (snd hb2)
