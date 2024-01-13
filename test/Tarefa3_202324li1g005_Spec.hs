module Tarefa3_202324li1g005_Spec where

import Tarefa3
import LI12324
import Test.HUnit


-- | Exemplo de blocos para testes
blocos1 :: [[Bloco]]
blocos1 = [ [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
          , [Vazio     ,Vazio     ,Vazio     ,Plataforma,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
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
          , [Vazio     ,Vazio     ,Vazio     ,Plataforma,Vazio     ,Vazio     ,Vazio     ,Vazio      ,Vazio     ,Vazio     ]
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
en4 = Personagem (-5.0,0.0) Fantasma (0.5,10.5) Oeste (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de inimigo que colide lateralmente com uma plataforma
en5 = Personagem (-5.0,0.0) Fantasma (4.5,1.5) Oeste (1,1) False True 1 0 (False, 0.0)

-- | Exemplo de inimigo que está na ponta da plataforma
en6 = Personagem (5.0,0.0) Fantasma (6.5,3.5) Este (1,1) False True 1 0 (False, 0.0)



-- | Exemplo de colecionavel
c1 = (Martelo, (4.5,4.5))
c2 = (Moeda, (3.5,7.5))

-- | Exemplo de jogo sem nenhuma consequência
j1 = Jogo gameMap1 [en2,en2] [c1,c2] pl1




teste1 = "T1: Não existe nenhuma consequência" ~: j1 ~=? movimenta 5 5.25 j1


-- Colisões de inimigos e jogador e efeitos aplicaDano

teste2 = "T2: O jogador armado atinge o fantasma" ~: j1{inimigos = [en1{vida=2},en2], jogador = pl1{pontos = 500, aplicaDano = (True,4.95)}}~=? movimenta 5 5.25 j1{inimigos = [en1,en2], jogador = pl1 {aplicaDano = (True,5)}}

teste3 = "T3: O jogador armado não atinge o fantasma" ~: j1{jogador = pl1{aplicaDano = (True,4.95)}} ~=? movimenta 5 5.25 j1{jogador = pl1{aplicaDano = (True,5)}}

teste4 = "T4: O jogador é atingido pelo inimigo" ~: j1{inimigos = [en1,en3], jogador = pl1{posicao = (8.5,6.5), direcao = Este, vida = 2}} ~=? movimenta 5 5.25 j1{inimigos = [en1,en3]}

-- Efeito da gravidade

teste5 = "T5: O jogador não está a pisar a plataforma" ~: j1{jogador =  pl1{velocidade = (0,10), posicao = (5.5,9.75)}} ~=? movimenta 5 5.25 j1{jogador = pl1 {posicao = (5.5,9.5)}}

-- Efeitos dos colecionaveis
teste6 = "T6: O jogador recolhe o martelo" ~: j1{colecionaveis = [c2], jogador = pl1 {posicao = (5,4.5), aplicaDano=(True,9.95)}} ~=? movimenta 5 5.25 j1{jogador = pl1{posicao = (5,4.5)}}

teste7 = "T7: O jogador recolhe a moeda" ~: j1 {colecionaveis = [c1], jogador = pl1 {posicao = (2.8,7.5), pontos = 200}} ~=? movimenta 5 5.25 j1{jogador = pl1 {posicao = (2.8,7.5)}}

-- Alçapão desaparece
teste8 = "T8: O jogador pisa o alçapão" ~: j1 {mapa = gameMap2, jogador = pl1 {posicao = (5,7.5)}} ~=? movimenta 5 5.25 j1 {jogador = pl1 {posicao = (5,7.5)}}

-- Colisões com a parede e as plataformas
teste9 = "T9: O jogador colide com o limite do mapa" ~: j1{jogador = pl1{posicao = (9.3,10.5)}} ~=? movimenta 5 5.25 j1{jogador = pl1 {posicao = (9.5,10.5)}}

teste10 = "T10: Um inimigo colide o limite do mapa" ~: j1{inimigos = [en4{velocidade = (5.0,0), posicao = (0.625,10.5)}, en2]} ~=? movimenta 5 5.25 j1{inimigos = [en4,en2]}

teste11 = "T11: Um personagem colide com a plataforma abaixo de si" ~: j1 ~=? movimenta 5 5.25 j1 {jogador = pl1 {velocidade = (0,10)}}

teste12 = "T12: O jogador colide lateralmente com uma plataforma" ~: j1{jogador = pl1 {velocidade = (-5,0),posicao = (4.575,1.5)}} ~=? movimenta 5 5.25 j1{jogador = pl1 {velocidade = (-5,0), posicao = (4.5,1.5)}}

teste13 = "T13: Um inimigo colide lateralmente com uma plataforma" ~: j1{inimigos = [en2,en5{velocidade = (5,0), posicao = (4.625,1.5)}]} ~=? movimenta 5 5.25 j1 {inimigos = [en2,en5]}

-- Movimento dos fantasmas 
teste14 = "T14: O fantasma altera a sua direção de maneira aleatoria" ~: j1{inimigos = [en2{velocidade = (1.5,0), posicao = (2.9625,10.5)},en2{velocidade = (1.5,0.0), tipo = Fantasma, posicao = (3.0375,10.5)}]} ~=? movimenta 1 1.15 j1 {inimigos = [en2{velocidade = (-1.5,0)}, en2{velocidade = (1.5,0)}]}

teste15 = "T15: O fantasma ressalta" ~: j1 {inimigos = [en2,en6{velocidade = (-5.0,0.0), posicao = (6.625,3.5)}]} ~=? movimenta 5 5.25 j1 {inimigos = [en2,en6]}


testesTarefa3 = test [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10,teste11,teste12,teste13,teste14,teste15]