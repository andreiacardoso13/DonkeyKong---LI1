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

{-| Define o menor retângulo que contém uma personagem.

= Exemplos

>>> hitbox (Personagem {velocidade = (2,2), tipo = Fantasma, posicao = (2.5,1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = ((2.0,0.5),(3.0,1.5))
>>> hitbox (Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (1,5.5), direcao = Sul, tamanho = (2,3), emEscada = False, ressalta = False, vida = 1, pontos = 0, aplicaDano = (False,0)}) = ((0.0,4.0),(2.0,7.0))
-}

hitbox :: Personagem -> Hitbox
hitbox (Personagem {posicao = (x,y), tamanho = (l,a)}) = ((x-(l/2),y-(a/2)),(x+(l/2), y+(a/2)))

{-| Define o menor retânculo que contém um colecionável

= Exemplos
>>> hitboxColecionavel (0.5,0.5) = ((0.0,0.0),(1.0,1.0))
>>> hitboxColecionavel (1,1) = ((0.5,0.5),(1.5,1.5)) 
-}

hitboxColecionavel :: Posicao -> Hitbox -- para ser usado apenas com colecionaveis
hitboxColecionavel (x,y) = ((x-0.5,y-0.5),(x+0.5, y+0.5))

{-|
Define a área onde um personagem consegue causar dano,
sendo esta do mesmo tamanho do menor retângulo que contém um personagem

=Exemplos
>>> hitboxDano (Personagem {posicao = (1,1), tamanho = (1,1), direcao = Este}) = ((1.5 , 0.5) , (2.5 , 1.5))
>>> hitboxDano (Personagem {posicao = (1,1), tamanho = (1,1), direcao = Oeste}) = ((-0.5 , 0.5) , (0.5 , 1.5))
-}

hitboxDano :: Personagem -> Hitbox
hitboxDano (Personagem {posicao = (x,y), tamanho = (l,a), direcao = dir}) | dir == Oeste || x < 0 = ((x-(2*l/2),y-(a/2)),(x, y + (a/2)))
                                                                          | dir == Este || x > 0 = ((x,y-(a/2)),(x + (2*l/2),y + (a/2)))


{-|
Verifica se duas hitbox estão em colisão

=Exemplos
>>>colisaoHitbox ((1,4),(3,1)) ((2,5),(4,3)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((2,2),(4,0)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((0,2),(4,0)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((4,2),(5,0)) = False
-}
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) | x1 >= x3 && x1 <= x4 && y2 >= y3 && y2 <= y4 = True
                                                  | x2 >= x3 && x2 <= x4 && y1 >= y3 && y1 <= y4 = True
                                                  | x1 >= x3 && x1 <= x4 && y1 >= y3 && y1 <= y4 = True
                                                  | x2 >= x3 && x2 <= x4 && y2 >= y3 && y2 <= y4 = True
                                                  | otherwise = False


{-|
Verifica se um número é natural

=Exemplos
>>>eNatural 2.0 = True
>>>eNatural 2.1 = False
-}
eNatural :: Double -> Bool
eNatural a | a < 0 = False
           | a == 0 = True
           | otherwise = eNatural (a-1)


{-| Testa se uma Personagem se encontra em colisão com os limites do Mapa.

= Exemplos

>>> mapaLimites mapaPrincipal (Personagem {velocidade = (-2,0), tipo = Fantasma, posicao = (0,0), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = True
>>> mapaLimites mapaPrincipal (Personagem {velocidade = (2,0), tipo = Fantasma, posicao = (4,3), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) = False
-}

mapaLimites :: Mapa -> Personagem -> Bool
mapaLimites m@(Mapa _ _ blocos) p = limDireito m p || limEsquerdo m p || bLim <= 0 || tLim >= fromIntegral mapaAltura
  where (eLim, bLim) = fst (hitbox p)
        (dLim, tLim) = snd (hitbox p)
        mapaLargura = length (head blocos)
        mapaAltura = length blocos
  -- "fromIntegral" faz os valores mapaLargura e mapaAltura serem doubles como os valores das coords

limDireito :: Mapa -> Personagem -> Bool
limDireito (Mapa _ _ blocos) p = dLim >= fromIntegral mapaLargura
      where (dLim, tLim) = snd (hitbox p)
            mapaLargura = length (head blocos)

limEsquerdo :: Mapa -> Personagem -> Bool
limEsquerdo (Mapa _ _ blocos) p = eLim <= 0
      where (eLim, bLim) = fst (hitbox p)


{-| Testa se uma Personagem se encontra em colisão com alguma Plataforma.

= Exemplos

>>> platColisoes (Mapa ((0,0), Este) (0,0) [[Plataforma,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (0.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False, 0)}) = True
>>> platColisoes (Mapa ((0,0), Este) (0,0) [[Plataforma,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2,1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 1, pontos = 100, aplicaDano = (False, 0)}) = False
-}

platColisoes :: Mapa -> Personagem -> Bool
platColisoes (Mapa _ _ blocos) p@(Personagem {posicao = (x,y),tamanho = (l,a)}) = blcI == Plataforma && mod' a 1 == 0 || blcS == Plataforma && mod' b 1 == 0 || blcD == Plataforma && mod' c 1 == 0 || blcE == Plataforma && mod' d 1 == 0
      where blcI = procuraBlocoInf blocos (x,y)
            blcS = procuraBlocoSup blocos (x,y)
            blcD = procuraBlocoDir blocos (x,y)
            blcE = procuraBlocoEsq blocos (x,y)
            a = snd(fst(hitbox p))
            b = snd(snd(hitbox p))
            c = fst(snd(hitbox p))
            d = fst(fst(hitbox p))

{-| Indica o tipo de Bloco situado abaixo do Bloco onde a personagem se encontra. -}

procuraBlocoInf :: [[Bloco]] -> Posicao -> Bloco
procuraBlocoInf ((h:t):ls) (x,y) | y > 0 = procuraBlocoInf ls (x,y-1)
                                 | x > 1 = procuraBlocoInf [t] (x-1,y)
                                 | otherwise = h
procuraBlocoInf _ _ = Vazio

{-| Indica o tipo de Bloco situado à direita do Bloco onde a personagem se encontra. -}

procuraBlocoDir :: [[Bloco]] -> Posicao -> Bloco
procuraBlocoDir ((h:t):ls) (x,y) | y > 1 = procuraBlocoDir ls (x,y-1)
                                 | x > 0 = procuraBlocoDir [t] (x-1,y)
                                 | otherwise = h
procuraBlocoDir _ _ = Vazio
{-| Indica o tipo de Bloco situado à esquerda do Bloco onde a personagem se encontra. -}

procuraBlocoEsq :: [[Bloco]] -> Posicao -> Bloco
procuraBlocoEsq ((h:t):ls) (x,y) | y > 1 = procuraBlocoEsq ls (x,y-1)
                                 | x > 2 = procuraBlocoEsq [t] (x-1,y)
                                 | otherwise = h
procuraBlocoEsq _ _ = Vazio

{-| Indica o tipo de Bloco situado acima do Bloco onde a personagem se encontra. -}

procuraBlocoSup :: [[Bloco]] -> Posicao -> Bloco
procuraBlocoSup ((h:t):ls) (x,y) | y > 2 = procuraBlocoSup ls (x,y-1)
                                 | x > 1 = procuraBlocoSup [t] (x-1,y)
                                 | otherwise = h
procuraBlocoSup _ _ = Vazio

{-| Dado um Mapa e uma Personagem, testa se a Personagem se encontra em colisão com os limites do Mapa ou com alguma Plataforma.

= Exemplos

>>> colisoesParede (Mapa ((0,0), Este) (0,0) [[Plataforma,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2.5,0.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 2, pontos = 350, aplicaDano = (False, 0)}) = True
>>> colisoesParede (Mapa ((0,0), Este) (0,0) [[Plataforma,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 2, pontos = 350, aplicaDano = (False, 0)}) = True
>>> colisoesParede (Mapa ((0,0), Este) (0,0) [[Plataforma,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2,1), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 2, pontos = 350, aplicaDano = (False, 0)}) = False
-}

colisoesParede :: Mapa -> Personagem -> Bool
colisoesParede m p = mapaLimites m p || platColisoes m p


{-| Testa se duas Personagens se encontram em colisão.

= Exemplos

>>> colisoesPersonagens (Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Sul, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}) = True
>>> colisoesPersonagens (Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (1,1.5), direcao = Sul, tamanho = (1,1), emEscada = True, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}) (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (2.5,1.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}) = False
-}

colisoesPersonagens :: Personagem -> Personagem -> Bool
colisoesPersonagens p1 p2 = colisaoHitbox (hitbox p1) (hitbox p2)

