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
import Data.Fixed

-- Personagem {velocidade = _, tipo = _, posicao = (x,y), direcao = _, tamanho = (l,a), emEscada = _, ressalta = _, vida = _, pontos = _, aplicaDano = _}
-- Personagem {velocidade = (2,2), tipo = Fantasma, posicao = (2.5,1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

{-| Define o menor retângulo que contém uma personagem ou objeto.

= Exemplos

>>> hitbox (Personagem {velocidade = (2,2), tipo = Fantasma, posicao = (2.5,1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = ((2.0,0.5),(3.0,1.5))
>>> hitbox (Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (1,5.5), direcao = Sul, tamanho = (2,3), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (False,0)}) = ((0.0,4.0),(2.0,7.0))
-}

hitbox :: Personagem -> Hitbox
hitbox (Personagem {posicao = (x,y), tamanho = (l,a)}) = ((x-(l/2),y-(a/2)),(x+(l/2), y+(a/2)))

{-| Testa se uma personagem se encontra em colisão com os limites do mapa.

= Exemplos

>>> mapaLimites mapaPrincipal (Personagem {velocidade = (-2,0), tipo = Fantasma, posicao = (0,0), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = True
>>> mapaLimites mapaPrincipal (Personagem {velocidade = (2,0), tipo = Fantasma, posicao = (4,3), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = False
-}

mapaLimites :: Mapa -> Personagem -> Bool
mapaLimites (Mapa _ _ blocos) p = eLim <= 0 || dLim >= fromIntegral mapaLargura || bLim <= 0 || tLim >= fromIntegral mapaAltura
  where (eLim, bLim) = fst (hitbox p)
        (dLim, tLim) = snd (hitbox p)
        mapaLargura = length (head blocos)
        mapaAltura = length blocos
  -- "fromIntegral" faz os valores mapaLargura e mapaAltura serem doubles como os valores das coords

{-| Testa se uma personagem se encontra em colisão com (em cima de) algum bloco de plataforma.

= Exemplos

>>> mapaChao (Mapa ((0,0), Este) (0,0) [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1.5,1.5), direcao = Norte, tamanho = (1,1), emEscada = True, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0)}) = True
>>> mapaChao (Mapa ((0,0), Este) (0,0) [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Este, tamanho = (0.5,0.5), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False, 0)}) = False
-}

mapaChao :: Mapa -> Personagem -> Bool
mapaChao (Mapa _ _ blocos) p@(Personagem {posicao = (x,y)}) = blc == Plataforma && mod' a 1 == 0
      where blc = procuraBlocoInf blocos (x,y)
            a = snd(fst(hitbox p))
-- hitbox is only defined for characters, not blocks!! 

{-| Indica o tipo de Bloco situado abaixo do Bloco onde a personagem se encontra.

= Exemplos

>>> procuraBlocoInf [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]] (1.5,1.5) = Plataforma
>>> procuraBlocoInf [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]] (2.5,0.5) = Vazio
-}

procuraBlocoInf :: [[Bloco]] -> Posicao -> Bloco
procuraBlocoInf ((h:t):ls) (x,y) | y > 0 = procuraBlocoInf ls (x,y-1)
                                 | x > 1 = procuraBlocoInf [t] (x-1,y)
                                 | otherwise = h

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m p = mapaLimites m p || mapaChao m p

{-| Testa se duas personagens se encontram em colisão.

= Exemplos

>>> colisoesPersonagens (Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Sul, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}) = True
>>> colisoesPersonagens (Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1,1.5), direcao = Sul, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}) = False
-}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = compHtbx hb1 hb2 || compHtbx hb2 hb1
    where hb1 = hitbox p1
          hb2 = hitbox p2
          compHtbx hb1 hb2 = fst (fst hb1) >= fst (fst hb2) && fst (fst hb1) <= fst (snd hb2) && snd (fst hb1) >= snd (fst hb2) && snd (fst hb1) <= snd (snd hb2)
