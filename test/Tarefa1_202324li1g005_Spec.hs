module Tarefa1_202324li1g005_Spec where

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
p7 = Personagem (0,0) Fantasma (9.5,10.5) Este (1,1) False True 1 0 (False, 0.0)
p8 = Personagem (0,0) Jogador (0.5,10.5) Este (1,1) False False 3 500 (False, 0.0)
p9 = Personagem (0,0) Fantasma (5,1.5) Este (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de blocos para testes
blocos1 :: [[Bloco]]
blocos1 = [ [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Escada    ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Escada    ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada     ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada     ,Vazio     ,Vazio     ]
          , [Vazio     ,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma ,Plataforma,Vazio     ]
          , [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Escada    ,Vazio     ]
          , [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Escada    ,Vazio     ]
          , [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma ,Plataforma,Plataforma]]
 
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

-- | Hitbox de dano
teste6 = "T6: Hitbox de dano do jogador" ~: ((5,3.5),(6,4.5)) ~=? hitboxDano p1

-- | Colisão entre personagens

teste7 = "T7: Duas hitboxes que não se encontram em colisão" ~: False ~=? colisoesPersonagens p1 p6

teste8 = "T8: Duas hitboxes que se encontram em colisão" ~: True ~=? colisoesPersonagens p5 p6

-- | Colisões com os limites do mapa

teste9 = "T9: Personagem em colisão com o limite direito do mapa" ~: True ~=? mapaLimites mapa1 p7

teste10 = "T10: Personagem em colisão com o limite esquerdo do mapa" ~: True ~=? mapaLimites mapa1 p8

teste11 = "T11: Personagem que não se encontra em colisão com limites do mapa" ~: False ~=? mapaLimites mapa1 p9

-- | Colisões com Plataformas

teste12 = "T12: Personagem em colisão com uma Plataforma" ~: True ~=? platColisoes mapa1 p9

teste13 = "T13: Personagem que não se encontra em colisão com paredes ou chão" ~: False ~=? colisoesParede mapa1 p3


testesTarefa1 = test [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10,teste11,teste12,teste13]
