module Tarefa1_202324li1g005_Spec (testesTarefa1) where

import Tarefa1
import LI12324
import Test.HUnit

-- | Exemplos de personagens para testes
p1 = Personagem (0,0) Jogador (5,4) Este (1,1) False False 10 0 (False, 0.0)
p2 = Personagem (0,0) Fantasma (4,4) Oeste (2,1) True False 2 0 (False, 0.0)
p3 = Personagem (0,0) Jogador (2,7) Este (1,1) False False 10 0 (False, 0.0)
p4 = Personagem (0,0) Fantasma (4,4) Oeste (2,2) True False 2 0 (False, 0.0)
p5 = Personagem (0,0) Jogador (3,2) Este (1,1) False False 10 0 (False, 0.0)
p6 = Personagem (0,0) Fantasma (3,3) Oeste (1,1) True False 2 0 (False, 0.0)

-- | Exemplo de blocos para testes
blocos1 :: [[Bloco]]
blocos1 = [ [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Vazio, Escada, Vazio, Vazio, Escada, Vazio, Vazio, Vazio]
          , [ Vazio, Vazio, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Escada, Vazio, Vazio]
          , [ Vazio, Plataforma, Plataforma, Plataforma, Alcapao, Plataforma, Plataforma, Plataforma, Plataforma, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Vazio, Escada, Vazio, Vazio, Vazio, Vazio, Vazio, Vazio, Escada, Vazio]
          , [ Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]]

-- | Exemplo de mapa para testes
mapa1 :: Mapa
mapa1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

-- | Hitbox de Personagens
teste1 = "T1: Hitbox personagem tamanho um por um" ~: ((4.5,3.5),(5.5,4.5)) ~=? hitbox p1

teste2 = "T1: Hitbox personagem tamanho dois por um" ~: ((3,3.5),(5,4.5)) ~=? hitbox p2

teste3 = "T3: Hitbox personagem tamanho dois por dois" ~: ((3,3),(5,5)) ~=? hitbox p4

-- | Hitbox de colecionaveis
teste4 = "T4: Hitbox de um colecionavel no meio de um bloco" ~: ((0,0),(1,1)) ~=? hitboxColecionavel (0.5,0.5)

teste5 = "T5: Hitbox de um colecionavel entre blocos" ~: ((0.5,0.5),(1.5,1.5)) ~=? hitboxColecionavel (1,1)





testesTarefa1 = test [teste1,teste2,teste3,teste4,teste5]
