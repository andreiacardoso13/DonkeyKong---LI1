module Tarefa3_202324li1g005_Spec where

import Tarefa3
import LI12324
import Test.HUnit


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
 
 -- | Exemplo de blocos para testes depois do jogador pisar o alçapão
blocos2 :: [[Bloco]]
blocos2 = [ [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Escada    ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Escada    ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada     ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada     ,Vazio     ,Vazio     ]
          , [Vazio     ,Plataforma,Plataforma,Plataforma,Vazio     ,Plataforma,Plataforma,Plataforma ,Plataforma,Vazio     ]
          , [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Escada    ,Vazio     ]
          , [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Escada    ,Vazio     ]
          , [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma ,Plataforma,Plataforma]]
 
-- | Exemplos de mapas para testes
gameMap1 :: Mapa
gameMap1 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos1

gameMap2 :: Mapa
gameMap2 = Mapa ((8.5, 6.5), Este) (5, 1.5) blocos2


-- | Exemplo de Jogador
pl1 = Personagem (0.0,0.0) Jogador (5.5,10.5) Oeste (1,1) False False 3 0 (False,0.0)

-- | Exemplo de inimigo que colide com hitbox de Dano
en1 = Personagem (0.0,0.0) Fantasma (4.5,10.5) Este (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de inimigo que não colide com o jogador nem a sua hitbox de Dano
en2 = Personagem (0.0,0.0) Fantasma (3,10.5) Este (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de inimigo que colide com o jogador
en3 = Personagem (0.0,0.0) Fantasma (4.5,10.5) Este (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de inimigo que colide com o mapa
en4 = Personagem (0.0,0.0) Fantasma (0.5,10.5) Este (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de colecionavel
c1 = (Martelo, (4.5,4.5))
c2 = (Moeda, (3.5,7.5))

-- | Exemplo de jogo sem nenhuma consequência
j1 = Jogo gameMap1 [en2,en2] [c1,c2] pl1




teste1 = "T1: Não existe nenhuma consequência" ~: j1 ~=? movimenta 5 5.25 j1


-- | Colisões de inimigos e jogadores

teste2 = "T2: O jogador armado atinge o fantasma" ~: j1{inimigos = [en1{vida=2},en2], jogador = pl1{pontos = 500, aplicaDano = (True,4.95)}}~=? movimenta 5 5.25 j1{inimigos = [en1,en2], jogador = pl1 {aplicaDano = (True,5)}}

teste3 = "T3: O jogador armado não atinge o fantasma" ~: j1{jogador = pl1{aplicaDano = (True,4.95)}} ~=? movimenta 5 5.25 j1{jogador = pl1{aplicaDano = (True,5)}}

teste4 = "T4: O jogador é atingido pelo inimigo" ~: j1{inimigos = [en1,en3], jogador = pl1{posicao = (8.5,6.5), direcao = Este, vida = 2}} ~=? movimenta 5 5.25 j1{inimigos = [en1,en3]}

-- | Efeito da gravidade

teste5 = "T5: O jogador não está a pisar a plataforma" ~: j1{jogador =  pl1{velocidade = (0,10), posicao = (5.5,9.75)}} ~=? movimenta 5 5.25 j1{jogador = pl1 {posicao = (5.5,9.5)}}

-- | Efeitos dos colecionaveis
teste6 = "T6: O jogador recolhe o martelo" ~: j1{colecionaveis = [c2], jogador = pl1 {posicao = (5,4.5), aplicaDano=(True,9.95)}} ~=? movimenta 5 5.25 j1{jogador = pl1{posicao = (5,4.5)}}

teste7 = "T7: O jogador recolhe a moeda" ~: j1 {colecionaveis = [c1], jogador = pl1 {posicao = (2.8,7.5), pontos = 200}} ~=? movimenta 5 5.25 j1{jogador = pl1 {posicao = (2.8,7.5)}}

-- | Alçapão desaparece
teste8 = "T8: O jogador pisa o alçapão" ~: j1 {mapa = gameMap2, jogador = pl1 {posicao = (5,7.5)}} ~=? movimenta 5 5.25 j1 {jogador = pl1 {posicao = (5,7.5)}}

-- | Colisões com o mapa e as plataformas
teste9 = "T9: O jogador colide com o mapa" ~: j1{jogador = pl1{posicao = (9.3,10.5)}} ~=? movimenta 5 5.25 j1{jogador = pl1 {posicao = (9.5,10.5)}}



testesTarefa3 = test [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9]















