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
import Tarefa1
import Tarefa2
import Tarefa4
import Mapa
import Data.Fixed
import Data.List

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta s t j = ressaltoFantasma $ movimentoPers $ movimentoMacaco t $ aleatoriedadeFantasmas s t $ alteraVidaFantasma $ tempoAplicaDano $ efeitoColisoes $ removeAlcapao $ recolheColecionavel $ ataqueDoInimigo $ efeitoGravidade $ ataqueDoJogador j
      
{-|
Se o jogador tiver a componente aplicaDano activa e com tempo restante e a 
hitbox de dano do jogador colidir com um fantasma retira uma vida ao fantasma

=Exemplos

>>>ataqueDoJogador (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]],inimigos = [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1.5,1.5),direcao = Oeste, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)},Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (4,1),direcao = Oeste, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}],colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))],jogador = Personagem {velocidade = (1,2),tipo = Jogador,posicao = (2.5,1),direcao = Oeste, tamanho = (1,2),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
   =Jogo {mapa = Mapa ((2.5,1.0),Norte) (2.5,1.0) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]],inimigos = [Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 0, pontos = 0, aplicaDano = (False,0.0)},Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (4.0,1.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}],colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))],jogador = Personagem {velocidade = (1.0,2.0), tipo = Jogador, posicao = (2.5,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5.0)}}
-}
ataqueDoJogador :: Jogo -> Jogo
ataqueDoJogador (Jogo {mapa = m ,inimigos = listaInimigos,colecionaveis = listaColecionaveis,jogador = jog}) 
        | fst (aplicaDano jog) == True && snd (aplicaDano jog) > 0 = Jogo {mapa = m 
                                                                          ,inimigos = ataqueDoJogadorInim listaInimigos jog
                                                                          ,colecionaveis = listaColecionaveis
                                                                          ,jogador = ataqueDoJogadorJog listaInimigos jog
                                                                          }
        | otherwise = (Jogo {mapa = m 
                            ,inimigos = listaInimigos 
                            ,colecionaveis = listaColecionaveis
                            ,jogador = jog
                            }
                      )

{-|
Se a hitbox de dano do jogador colidir com um inimigo retira uma vida ao inimigo

=Exemplos
>>>ataqueDoJogadorInim [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (7,7),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)},Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}] (Personagem {posicao = (0.5,1),direcao = Este,tamanho = (1,2),aplicaDano = (True,5)})
   =[Personagem {velocidade = (1.0,2.0),tipo = Fantasma,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)},Personagem {velocidade = (1.0,2.0),tipo = Fantasma,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}]
-}
ataqueDoJogadorInim :: [Personagem] -> Personagem -> [Personagem]
ataqueDoJogadorInim [] _ = []
ataqueDoJogadorInim (inim :t) jog | colisaoHitbox (hitbox inim) (hitboxDano jog) && (tipo inim) == Fantasma && (vida inim) == 1 = (inim {vida = vida inim - 1}) : ataqueDoJogadorInim t jog
                                  | otherwise = inim : ataqueDoJogadorInim t jog
        
{-| 
Se a hitbox de dano do jogador colidir com um inimigo adiciona 500 pontos ao jogador

=Exemplo
>>>ataqueDoJogadorJog [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}] (Personagem {velocidade = (1,0),tipo = Jogador,posicao = (0.5,1),direcao = Este,tamanho = (1,2),emEscada = False,ressalta = False,vida=3,pontos=0,aplicaDano = (True,5)})
   =Personagem {velocidade = (1.0,0.0), tipo = Jogador, posicao = (0.5,1.0), direcao = Este, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 1000, aplicaDano = (True,5.0)}
-}
ataqueDoJogadorJog :: [Personagem] -> Personagem -> Personagem
ataqueDoJogadorJog [] jog = jog
ataqueDoJogadorJog (inim :t) jog | colisaoHitbox (hitbox inim) (hitboxDano jog) && (tipo inim) == Fantasma && vida inim == 1 = ataqueDoJogadorJog t (jog {pontos = pontos jog + 500})
                                 | otherwise = ataqueDoJogadorJog t jog

{-|
Se os personagens estiverem em cima dum bloco "Vazio"
altera a sua velocidade de maneira a sofrerem efeito da gravidade

=Exemplos
>>>efeitoGravidade (Jogo {mapa = Mapa ((2.5 , 1),Oeste) (2.5,1) [[Vazio,Vazio], [Vazio,Plataforma], [Vazio,Vazio], [Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1,0), tipo = Fantasma,posicao = (1.5,0.5),direcao = Este, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}],colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))],jogador = Personagem {velocidade = (1,0),tipo = Jogador,posicao = (0.5,0.5),direcao = Este, tamanho = (1,1),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
   =Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))], jogador = Personagem {velocidade = (0.0,10.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5.0)}}

-}
efeitoGravidade :: Jogo -> Jogo
efeitoGravidade (Jogo {mapa = (Mapa posI posF matriz) 
                      ,inimigos = listaInimigos
                      ,colecionaveis = listaColecionaveis
                      ,jogador = jog}) = (Jogo {mapa = (Mapa posI posF matriz)
                                               ,inimigos = efeitoGravidadeInimigos matriz listaInimigos
                                               ,colecionaveis = listaColecionaveis
                                               ,jogador = efeitoGravidadeJogador matriz jog})

{-|
Verifica onde estão os inimigos e se estiverem em cima do bloco 
"Vazio" altera a sua velocidade de maneira a sofrerem efeito da gravidade

=Exemplos
>>> efeitoGravidadeInimigos [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]] [Personagem {velocidade = (0,2),tipo = Fantasma, posicao = (1.5,0.5),direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
   =[Personagem {velocidade = (0.0,2.0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
>>> efeitoGravidadeInimigos [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]] [Personagem {velocidade = (0,2),tipo = Fantasma, posicao = (0.5,0.5),direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
   =[Personagem {velocidade = (0.0,10.0), tipo = Fantasma, posicao = (0.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
-}
efeitoGravidadeInimigos :: [[Bloco]] -> [Personagem] -> [Personagem]
efeitoGravidadeInimigos m [] = []
efeitoGravidadeInimigos m (inim : t) | procuraBlocoInf m (posicao inim) == Vazio && tipo inim == Fantasma = inim {velocidade = (fst (velocidade inim), snd gravidade)} : efeitoGravidadeInimigos m t
                                     | otherwise = inim : efeitoGravidadeInimigos m t

{-|
Verifica onde estão o jogador e se estiver em cima do bloco 
"Vazio" altera a sua velocidade de modo a sofrerem efeito da gravidade

=Exemplos
>>> efeitoGravidadeJogador [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]] (Personagem {velocidade = (2,0),tipo = Jogador, posicao = (0.5,0.5),direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (0.0,10.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}
-}
efeitoGravidadeJogador :: [[Bloco]] -> Personagem -> Personagem
efeitoGravidadeJogador m jog | procuraBlocoInf m (posicao jog) == Vazio = jog {velocidade = (fst (velocidade jog), snd gravidade)}
                             | otherwise = jog

{-|
Se o jogador colidir com o inimigo retira-lhe uma vida e altera a sua posição para a inicial

=Exemplos
>>>ataqueDoInimigo (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]],inimigos = [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Oeste, tamanho = (2,2),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}],colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))],jogador = Personagem {velocidade = (1,2),tipo = Jogador,posicao = (2,2),direcao = Oeste, tamanho = (2,2),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
              =Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))], jogador = Personagem {velocidade = (1.0,2.0), tipo = Jogador, posicao = (2.0,2.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = False, vida = 2, pontos = 0, aplicaDano = (True,5.0)}}
-}
ataqueDoInimigo :: Jogo -> Jogo
ataqueDoInimigo j = j {jogador = ataqueDoInimigoAux j}

{-|
Se o jogador colidir com um inimigo retira-lhe uma vida e altera a sua posição para a inicial

=Exemplos
>>> ataqueDoInimigoAux [Personagem {posicao = (1,1),tamanho = (2,2),vida = 1}] (Personagem {velocidade = (1,2),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos= 0,aplicaDano = (False,0)})
   = Personagem {velocidade = (1.0,2.0),tipo = Jogador,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}
-}
ataqueDoInimigoAux :: Jogo -> Personagem 
ataqueDoInimigoAux j | inimigos j == [] = jogador j
                     | colisoesPersonagens inim jog && (vida inim == 1 ) && (vida jog <= 3) && (vida jog >1)= (jog {posicao = pos , velocidade = (0,0),direcao = Este,emEscada = False, vida = vida jog -1})
                     | colisoesPersonagens inim jog && (vida inim == 1 ) && (vida jog <= 3) && (vida jog >0)= (jog {vida = vida jog -1})
                     | otherwise = ataqueDoInimigoAux j{inimigos = drop 1 (inimigos j)}
  where (inim:t) = inimigos j 
        jog = jogador j
        Mapa (pos,dir) a b = mapa j

{-|
Reflete as consequências de quando um colecionável é recolhido

=Exemplos
>>>recolheColecionavel (Jogo {mapa= Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1,0),tipo = Fantasma,posicao=(0.5,1.5),direcao =Este,tamanho = (1,1),emEscada = False,ressalta=True,vida = 1, pontos = 0, aplicaDano = (False,0)}], colecionaveis = [(Moeda,(1.5,1.5)),(Martelo,(0.5,1.5))], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
   =Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(0.5,1.5))], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 200, aplicaDano = (False,0.0)}}
>>>recolheColecionavel (Jogo {mapa= Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1,0),tipo = Fantasma,posicao=(0.5,1.5),direcao =Este,tamanho = (1,1),emEscada = False,ressalta=True,vida = 1, pontos = 0, aplicaDano = (False,0)}], colecionaveis = [(Moeda,(0.5,1.5)),(Martelo,(1.5,1.5))], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
   =Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(0.5,1.5))], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,10.0)}}
-}
recolheColecionavel :: Jogo -> Jogo 
recolheColecionavel (Jogo {mapa = m ,inimigos = listaInimigos,colecionaveis = listaColecionaveis,jogador = jog}) = Jogo {mapa = m 
                                                                                                                        ,inimigos = listaInimigos
                                                                                                                        ,colecionaveis = removeColecionavel listaColecionaveis jog
                                                                                                                        ,jogador = efeitoColecionavel listaColecionaveis jog}

{-|
Se houver colisão entre um personagem e um colecionável remove o colécionável da lista

=Exemplos
>>> removeColecionavel [(Moeda,(1.5,1.5)),(Martelo,(0.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   =[(Martelo,(0.5,1.5))]
>>> removeColecionavel [(Moeda,(0.5,1.5)),(Martelo,(1.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   =[(Moeda,(0.5,1,5))]
-}
removeColecionavel :: [(Colecionavel, Posicao)] -> Personagem -> [(Colecionavel, Posicao)]
removeColecionavel [] jog = []
removeColecionavel (h:t) jog | colisaoHitbox (hitboxColecionavel (snd h)) (hitbox jog) = removeColecionavel t jog
                             | otherwise = h : removeColecionavel t jog

{-|
Devolve o personagem com as consequência de recolher um colecionável
(apenas de este o recolher)

=Exemplos
>>> efeitoColecionavel [(Moeda,(1.5,1.5)),(Martelo,(0.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   =Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 200, aplicaDano = (False,0.0)}
>>> efeitoColecionavel [(Moeda,(0.5,1.5)),(Martelo,(1.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   =Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,10.0)}
-}
efeitoColecionavel :: [(Colecionavel, Posicao)] -> Personagem -> Personagem
efeitoColecionavel [] jog = jog
efeitoColecionavel (h:t) jog | colisaoHitbox (hitboxColecionavel (snd h)) (hitbox jog) = if fst h == Moeda
                                                                                            then efeitoColecionavel t (jog {pontos = pontos jog + 200})
                                                                                            else efeitoColecionavel t (jog {aplicaDano = (True,10)})
                             | otherwise = efeitoColecionavel t jog

{-|
Faz o Alçapão desaparecer caso este esteja a ser pisado pelo Jogador

=Exemplos
>>>removeAlcapao (Jogo {mapa = Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2.6,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
   = Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.6,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
>>>removeAlcapao (Jogo {mapa= Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (1.5,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
   =Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Vazio,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (1.5,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}} 
>>>removeAlcapao (Jogo {mapa= Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (0.6,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
   =Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Vazio,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (0.6,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
-}
removeAlcapao :: Jogo -> Jogo
removeAlcapao (Jogo {mapa= Mapa a b blocos ,inimigos = listaInimigos,colecionaveis = listaColecionaveis,jogador = jog}) = (Jogo {mapa = Mapa a b (removeAlcapaoEsquerda (removeAlcapaoDireita blocos (hitbox jog)) (hitbox jog)),inimigos = listaInimigos,colecionaveis = listaColecionaveis,jogador = jog})

{-|
Faz o Alçapão desaparecer caso este esteja a ser pisado pela parte direita do Jogador

=Exemplos
>>> removeAlcapaoDireita [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]] ((0.5,0),(1.5,1)) = [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Plataforma]]
>>> removeAlcapaoDireita [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]] ((1,0),(2,1)) = [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Plataforma]]
-}
removeAlcapaoDireita :: [[Bloco]] -> Hitbox -> [[Bloco]]
removeAlcapaoDireita [] _ = []
removeAlcapaoDireita (h:t) ((x1,y1),(x2,y2)) | eNatural y2 && y2 == 0 && estaEmAlcapao h (x2,y2) = blocoParaVazio h (x2,y2) :t
                                         | eNatural y2 && y2 == 0 = (h:t)
                                         | eNatural y2 = h : removeAlcapaoDireita t ((x1,y1-1),(x2,y2-1))
                                         | otherwise = (h:t)

{-|
Faz o Alçapão desaparecer caso este esteja a ser pisado pela parte esquerda do Jogador

=Exemplos
>>>removeAlcapaoEsquerda [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]] ((1.1,0),(2.1,1)) = [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Plataforma]]
>>>removeAlcapaoEsquerda [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]] ((0.5,0),(1.5,1)) = [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Plataforma]]
-}
removeAlcapaoEsquerda :: [[Bloco]] -> Hitbox -> [[Bloco]]
removeAlcapaoEsquerda [] _ = []
removeAlcapaoEsquerda (h:t) ((x1,y1),(x2,y2)) | eNatural y2 && y2 == 0 && estaEmAlcapao h (x1,y1) = blocoParaVazio h (x1,y1) :t
                                              | eNatural y2 && y2 == 0 = (h:t)
                                              | eNatural y2 = h : removeAlcapaoEsquerda t ((x1,y1-1),(x2,y2-1))
                                              | otherwise = (h:t)

{-|
Verifica se o Bloco onde se localiza o x da posição recebida é um Alcapão

=Exemplos
>>>estaEmAlcapao [Vazio,Alcapao,Plataforma] (0.5,5) = False
>>>estaEmAlcapao [Vazio,Alcapao,Plataforma] (1,5) = True
>>>estaEmAlcapao [Vazio,Alcapao,Plataforma] (2,5) = True
-}
estaEmAlcapao :: [Bloco] -> Posicao -> Bool
estaEmAlcapao [] _ = False
estaEmAlcapao (h:t) (x,y) | x > 0 && x <= 1 && h == Alcapao = True
                          | otherwise = estaEmAlcapao t (x-1,y)

{-|
Altera o Bloco onde se localiza o x da posição recebida para Vazio

=Exemplos
>>> blocoParaVazio [Plataforma,Plataforma,Alcapao] (0.5,6) = [Vazio,Plataforma,Alcapao]
>>> blocoParaVazio [Plataforma,Plataforma,Alcapao] (2,6) = [Plataforma,Vazio,Alcapao]
>>> blocoParaVazio [Plataforma,Plataforma,Alcapao] (1.5,6) = [Plataforma,Vazio,Alcapao]
-}
blocoParaVazio :: [Bloco] -> Posicao -> [Bloco] 
blocoParaVazio [] _ = []
blocoParaVazio (h:t) (x,y) | x >= 0 && x <= 1 && h == Alcapao = Vazio : t
                           | otherwise = h : blocoParaVazio t (x-1,y)

{-|
Devolve o jogo recebido com as consequências de colisões dos personagens com o mapa e/ou plataformas

=Exemplos
>>> efeitoColisoes (Jogo {mapa = Mapa ((0.5,0.5),Este) (2.5,0.5) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem{velocidade = (-1.5,0),tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1,1),emEscada = False,ressalta=True,vida=1,pontos=0,aplicaDano=(False,0)}], colecionaveis=[],jogador = Personagem{velocidade = (0,0), tipo = Jogador, posicao = (2.5,1.5),direcao=Este,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}})
   = Jogo {mapa = Mapa ((0.5,0.5),Este) (2.5,0.5) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.5,0.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (2.3,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
>>>efeitoColisoes (Jogo {mapa = Mapa ((0.5,0.5),Este) (2.5,0.5) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem{velocidade = (1.5,0),tipo = Fantasma, posicao = (2.5,1.5), direcao = Este, tamanho = (1,1),emEscada = False,ressalta=True,vida=1,pontos=0,aplicaDano=(False,0)}], colecionaveis=[],jogador = Personagem{velocidade = (0,0), tipo = Jogador, posicao = (1.5,1.5),direcao=Este,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}})
   = Jogo {mapa = Mapa ((0.5,0.5),Este) (2.5,0.5) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (-1.5,0.0), tipo = Fantasma, posicao = (2.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.7,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
-}
efeitoColisoes :: Jogo -> Jogo
efeitoColisoes jogo' = jogo' {inimigos = efeitoColisoesInimigos (mapa jogo') (inimigos jogo'), jogador = efeitoColisoesJogador (mapa jogo') (jogador jogo')}

{-|
Devolve o jogador com as consequências de colisões do jogador com o mapa e/ou plataformas

=Exemplos
>>> efeitoColisoesJogador (Mapa ((0.5,0.5),Este) (2.5,0.5) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (0,0), tipo = Jogador, posicao = (2.5,1.5),direcao=Este,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (2.3,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
>>> efeitoColisoesJogador (Mapa ((0.5,0.5),Este) (2.5,0.5) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (0,0), tipo = Jogador, posicao = (1.5,1.5),direcao=Este,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.7,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}
efeitoColisoesJogador :: Mapa -> Personagem -> Personagem
efeitoColisoesJogador m jog = efeitoColisoesPlataforma2 m $ efeitoColisoesPlataforma1 m $ efeitoColisoesMapaJog m jog

{-|
Devolve a lista de inimigos com as consequências de colisões dos mesmos com o mapa e/ou plataformas

=Exemplos
>>>efeitoColisoesInimigos ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) [Personagem{velocidade = (-1.5,0), tipo = Fantasma, posicao = (1.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0)}]
   = [Personagem {velocidade = (1.5,0.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0.0)}]
>>>efeitoColisoesInimigos ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) [Personagem{velocidade = (-1.5,0), tipo = Fantasma, posicao = (2.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0)}]
   = [Personagem {velocidade = (1.5,0.0), tipo = Fantasma, posicao = (2.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0.0)}]
-}
efeitoColisoesInimigos :: Mapa -> [Personagem] -> [Personagem]
efeitoColisoesInimigos _ [] = []
efeitoColisoesInimigos m (h:t) = map (efeitoColisoesPlataforma2 m) (map (efeitoColisoesPlataforma1 m) (map (efeitoColisoesMapaInim m) (h:t)))

{-|
Devolve um personagem com as consequências de colidir com uma plataforma abaixo de si

=Exemplos
>>>efeitoColisoesPlataforma1 ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (0,10), tipo = Fantasma, posicao = (2.5,1.5),direcao=Sul,tamanho = (1,1), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (0.0,0.0), tipo = Fantasma, posicao = (2.5,1.5), direcao = Sul, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
>>>efeitoColisoesPlataforma1 ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (0,10), tipo = Fantasma, posicao = (2.5,0.5),direcao=Sul,tamanho = (1,1), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (0.0,10.0), tipo = Fantasma, posicao = (2.5,0.5), direcao = Sul, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}
efeitoColisoesPlataforma1 :: Mapa -> Personagem -> Personagem
efeitoColisoesPlataforma1 (Mapa a b blocos) pers | platColisoes (Mapa a b blocos) pers && procuraBlocoInf blocos (posicao pers) == Plataforma = pers {velocidade = (vx,0)}
                                                 | otherwise = pers
  where (vx,vy) = velocidade pers

{-|
Devolve um personagem com as consequências de colidir lateralmente com uma plataforma

=Exemplos
>>>efeitoColisoesPlataforma2 ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (-1.5,0), tipo = Fantasma, posicao = (1.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (1.5,0.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
>>>efeitoColisoesPlataforma2 ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Plataforma,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (-1.5,0), tipo = Jogador, posicao = (1.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (-1.5,0.0), tipo = Jogador, posicao = (1.7,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}
efeitoColisoesPlataforma2 :: Mapa -> Personagem -> Personagem
efeitoColisoesPlataforma2 (Mapa a b blocos) pers| platColisoes (Mapa a b blocos) pers && procuraBloco blocos (x+0.5,y) == Plataforma = if tipo pers == Jogador 
                                                                                                                                                  then pers {posicao = (x-0.2,y)}
                                                                                                                                                  else pers {velocidade = (-vx,vy)}
                                                | platColisoes (Mapa a b blocos) pers && procuraBloco blocos (x-0.5,y) == Plataforma = if tipo pers == Jogador
                                                                                                                                                 then pers {posicao = (x+0.2,y)}
                                                                                                                                                 else pers {velocidade = (-vx,vy)}
                                                | otherwise = pers
  where (vx,vy) = velocidade pers
        (x,y) = posicao pers

{-|
Devolve um jogador com as consequências de colidir com os limites do mapa

=Exemplos
>>> efeitoColisoesMapaJog ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (-1.5,0), tipo = Jogador, posicao = (2.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (-1.5,0.0), tipo = Jogador, posicao = (2.3,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
>>> efeitoColisoesMapaJog ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (-1.5,0), tipo = Jogador, posicao = (0.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (-1.5,0.0), tipo = Jogador, posicao = (0.7,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}
efeitoColisoesMapaJog :: Mapa -> Personagem -> Personagem
efeitoColisoesMapaJog m jog | mapaLimites m jog = if fst(posicao jog) > ((fromIntegral (length (head blocos))) / 2)
                                                    then jog {posicao = (fst (posicao jog) -0.2, snd (posicao jog))}
                                                    else jog {posicao = (fst (posicao jog) +0.2, snd (posicao jog))}
                            | otherwise = jog
  where Mapa a b blocos = m

{-|
Devolve um jogador com as consequências de colidir com os limites do mapa

=Exemplos
>>> efeitoColisoesMapaInim ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (-1.5,0), tipo = Fantasma, posicao = (0.5,1.5),direcao=Oeste,tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (1.5,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}
>>> efeitoColisoesMapaInim ((Mapa ((0.5,0.5),Este) (2.5,0.5)) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]]) (Personagem{velocidade = (1.5,0), tipo = Fantasma, posicao = (2.5,1.5),direcao=Este,tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)})
   = Personagem {velocidade = (-1.5,0.0), tipo = Fantasma, posicao = (2.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}
-}
efeitoColisoesMapaInim :: Mapa -> Personagem -> Personagem
efeitoColisoesMapaInim m inim | mapaLimites m inim = inim {velocidade = (-vx,vy)}
                              | otherwise = inim
  where (vx,vy) = velocidade inim

-- faz o efeito do tempo no parâmetro aplicaDano do jogador

{-|
Faz o tempo passar no parâmetro aplicaDano do jogador

=Exemplos
>>> tempoAplicaDano (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [], colecionaveis = [], jogador = (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(True,10)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,9.95)}}
>>> tempoAplicaDano (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [], colecionaveis = [], jogador = (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
-}
tempoAplicaDano :: Jogo -> Jogo
tempoAplicaDano (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog}) | snd (aplicaDano jog) <= 0 = (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog {aplicaDano = (False, 0)}})
                                                                                       | snd (aplicaDano jog) > 0 = (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog {aplicaDano = (True, snd (aplicaDano jog) - 0.05)}})
                                                                                       | otherwise = (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog})
{-|
Altera a vida dos fantasmas que foram atingidos pelo jogador
Faz parte da estratégia utilizada para fazer a animação de explosão dos fantasmas

=Exemplos
>>>alteraVidaFantasma (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [(Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=0,pontos=0,aplicaDano=(False,0)})], colecionaveis = [], jogador = (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
>>>alteraVidaFantasma (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [(Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=7,pontos=0,aplicaDano=(False,0)})], colecionaveis = [], jogador = (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 8, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
>>>alteraVidaFantasma (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [(Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=11,pontos=0,aplicaDano=(False,0)})], colecionaveis = [], jogador = (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 11, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
-}
alteraVidaFantasma :: Jogo -> Jogo
alteraVidaFantasma jog = jog {inimigos = map (alteraVidaFantasmaAux) (inimigos jog)}

{-|
Altera a vida dos fantasmas que foram atingidos pelo jogador
Faz parte da estratégia utilizada para fazer a animação de explosão dos fantasmas

=Exemplos
>>> alteraVidaFantasmaAux (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=0,pontos=0,aplicaDano=(False,0)})
   = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 2, pontos = 0, aplicaDano = (False,0.0)}
>>> alteraVidaFantasmaAux (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=7,pontos=0,aplicaDano=(False,0)})
   = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 8, pontos = 0, aplicaDano = (False,0.0)}
>>> alteraVidaFantasmaAux (Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=11,pontos=0,aplicaDano=(False,0)})
   = Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 11, pontos = 0, aplicaDano = (False,0.0)}
-}
alteraVidaFantasmaAux :: Personagem -> Personagem
alteraVidaFantasmaAux inim | vida inim == 0 = inim {vida = 2}
                           | vida inim >= 2 && vida inim <= 10 =inim {vida = (vida inim) + 1}
                           | otherwise = inim 

{-|
Faz o MacacoMalvado cair enquanto não colide com o chão do mapa (utilizada para animção final do jogo)

=Exemplos
>>> gravidadeMacaco 0.05 [Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (0,0),direcao = Este,tamanho=(2,2),emEscada=False,ressalta=True,vida=1,pontos=0,aplicaDano=(False,0)}]
   = [Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (0.0,0.2), direcao = Este, tamanho = (2.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
>>> gravidadeMacaco 0.05 [Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (0,16.2),direcao = Este,tamanho=(2,2),emEscada=False,ressalta=True,vida=1,pontos=0,aplicaDano=(False,0)}]
   = [Personagem {velocidade = (0.0,0.0), tipo = MacacoMalvado, posicao = (0.0,16.2), direcao = Este, tamanho = (2.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
-}
gravidadeMacaco :: Tempo -> [Personagem] -> [Personagem]
gravidadeMacaco _ [] = []
gravidadeMacaco tmp (h:t) | tipo h == MacacoMalvado && snd (posicao h) <= 16.1 = (h {posicao = (fst (posicao h), snd (posicao h) + 4 * tmp)}) : t
                          | otherwise = h : gravidadeMacaco tmp t 

{-|
Responsavel por alterar a posição dos personagens quando estes têm velocidade diferente de 0

=Exemplos
>>> movimentoPers (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [(Personagem {velocidade = (0,10), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=11,pontos=0,aplicaDano=(False,0)})], colecionaveis = [], jogador = (Personagem {velocidade = (5,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,10.0), tipo = Jogador, posicao = (1.0,1.75), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 11, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (5.0,0.0), tipo = Jogador, posicao = (1.125,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
>>> movimentoPers (Jogo {mapa = Mapa ((0,0),Este) (1,1) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [(Personagem {velocidade = (0,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=True,vida=11,pontos=0,aplicaDano=(False,0)})], colecionaveis = [], jogador = (Personagem {velocidade = (10,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})})
   = Jogo {mapa = Mapa ((0.0,0.0),Este) (1.0,1.0) [[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (0.0,0.0), tipo = Jogador, posicao = (1.0,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 11, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [], jogador = Personagem {velocidade = (10.0,0.0), tipo = Jogador, posicao = (1.25,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}}
-}
movimentoPers :: Jogo -> Jogo
movimentoPers jogo = jogo {mapa = mapa jogo, inimigos = map movimentoPersAux (inimigos jogo) , colecionaveis = colecionaveis jogo, jogador = movimentoPersAux (jogador jogo)}

{-|
Altera a posição dum personagem quando este tem velocidade diferente de 0

=Exemplos
>>> movimentoPersAux (Personagem {velocidade = (10,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   = Personagem {velocidade = (10.0,0.0), tipo = Jogador, posicao = (1.25,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
>>> movimentoPersAux (Personagem {velocidade = (1,0), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   = Personagem {velocidade = (1.0,0.0), tipo = Jogador, posicao = (1.025,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
>>> movimentoPersAux (Personagem {velocidade = (5,5), tipo = Jogador, posicao = (1,1.5),direcao = Este,tamanho=(1,1),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
   = Personagem {velocidade = (5.0,5.0), tipo = Jogador, posicao = (1.125,1.625), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0.0)}
-}
movimentoPersAux :: Personagem -> Personagem
movimentoPersAux pers = pers {posicao = (x + vx*0.025,y + vy *0.025)}
   where (vx,vy) = velocidade pers
         x = fst(posicao pers)
         y = snd(posicao pers) 

-- | Responsavel pela aleatoriedade no movimento dos Fantasmas
aleatoriedadeFantasmas :: Semente -> Tempo -> Jogo -> Jogo
aleatoriedadeFantasmas s t j = j{inimigos = fantEscada j {inimigos = (aleatFantEscada s t j {inimigos = aleatFantAndar s t j})}}


aleatFantEscada :: Semente -> Tempo -> Jogo -> [Personagem]
aleatFantEscada _ _ j@(Jogo {inimigos = []}) = []
aleatFantEscada s tp j@(Jogo {mapa = m@(Mapa _ _ blocos), inimigos = (h@(Personagem {posicao = pos@(x,y), velocidade = (vx,vy), emEscada = esc}):t)}) 

      | alteraImagem3 (realToFrac tp) && (head(geraAleatorios s 1)) > 0 = fantEscadaSubir j
      | alteraImagem3 (realToFrac tp) && (head(geraAleatorios s 1)) < 0 = fantEscadaDescer j
      | otherwise = h : aleatFantEscada (s+5) tp j{inimigos = t}
                           

fantEscada :: Jogo -> [Personagem]
fantEscada j@(Jogo {inimigos = []}) = []
fantEscada j@(Jogo {mapa = m@(Mapa _ _ blocos), inimigos = (h@(Personagem {posicao = pos@(x,y), velocidade = (vx,vy), emEscada = esc}):t)})

      | tipo h == Fantasma && esc && procuraBloco blocos pos == Escada && procuraBlocoInf blocos pos == Plataforma && colisoesParede m h && mod' x 1 <= 0.55 && mod' x 1 >= 0.45 && vy > 0 = (h{velocidade = (1.5,0),  emEscada = False, posicao = (x+0.15,y)}) : fantEscada j{inimigos = t}
      | tipo h == Fantasma && esc && procuraBloco blocos pos == Vazio  && procuraBlocoInf blocos pos == Plataforma && colisoesParede m h && mod' x 1 <= 0.55 && mod' x 1 >= 0.45 && vy < 0 = (h{velocidade = (-1.5,0), emEscada = False, posicao = (x-0.15,y)}) : fantEscada j{inimigos = t}
      | otherwise = h : fantEscada j{inimigos = t}

fantEscadaSubir :: Jogo -> [Personagem]
fantEscadaSubir j@(Jogo {inimigos = []}) = []
fantEscadaSubir j@(Jogo {mapa = m@(Mapa _ _ blocos), inimigos = (h@(Personagem {posicao = pos@(x,y), velocidade = (vx,vy), emEscada = esc}):t)})

      | tipo h == Fantasma && not esc && procuraBloco blocos pos == Escada && procuraBlocoInf blocos pos == Plataforma && colisoesParede m h && mod' x 1 <= 0.55 && mod' x 1 >= 0.45 = (h{velocidade = (0,-1.5), emEscada = True, posicao = (x,y-0.1)}) : fantEscadaSubir j{inimigos = t}
      | otherwise = h : fantEscadaSubir j{inimigos = t}


fantEscadaDescer :: Jogo -> [Personagem]
fantEscadaDescer j@(Jogo {inimigos = []}) = []
fantEscadaDescer j@(Jogo {mapa = m@(Mapa _ _ blocos), inimigos = (h@(Personagem {posicao = pos@(x,y), velocidade = (vx,vy), emEscada = esc}):t)})

      | tipo h == Fantasma && not esc && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos (x,y+2) == Escada && mod' x 1 <= 0.55 && mod' x 1 >= 0.45 = (h{velocidade = (0,1.5), emEscada = True, posicao = (x,y+0.1)}) : fantEscadaDescer j{inimigos = t}
      | otherwise = h : fantEscadaDescer j{inimigos = t}

{-
CONDIÇOES SUBIR
|     esc && procuraBloco blocos pos == Vazio && procuraBlocoInf blocos pos == Plataforma && colisoesParede m jgd = return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Parar) j {jogador = jgd {emEscada = False}}}
| not esc && procuraBloco blocos pos == Escada && mod' x 1 <= 0.6 && mod' x 1 >= 0.3                              = return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Subir) j}
| esc                                                                                                             = return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Subir) j {jogador = jgd {posicao = (x, max (y-0.5) 0.5)}}}

CONDIÇOES DESCER
|esc  && procuraBlocoInf blocos pos == Escada                                                                       =  freefall (e{jogo = j {jogador = movePersonagem (jgd {posicao = (x, min (y+0.5) 16.5)}) (Just Descer)}})
|esc  && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos pos     == Escada && colisoesParede m jgd  =  freefall (e{jogo = j {jogador = movePersonagem (jgd {emEscada = False}) (Just Parar)}})
|        procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos (x,y+2) == Escada && mod' x 1 /= 0         =  freefall (e{jogo = j {jogador = movePersonagem (jgd {posicao = (x, min (y+0.5) 16.5)}) (Just Descer)}})
|        procuraBlocoInf blocos pos == Escada     && procuraBloco blocos pos     == Plataforma                      =  freefall (e{jogo = j {jogador = movePersonagem jgd (Just Descer)}})
-}

aleatFantAndar :: Semente -> Tempo -> Jogo -> [Personagem]
aleatFantAndar _ _ j@(Jogo {inimigos = []}) = []
aleatFantAndar s tp j@(Jogo {mapa = m@(Mapa _ _ blocos), inimigos = (h@(Personagem {posicao = pos@(x,y), velocidade = (vx,vy), emEscada = esc}):t)}) 

      | alteraImagem3 (realToFrac tp) && (head(geraAleatorios s 1)) > 0 && tipo h == Fantasma && not esc && colisoesParede m h = (h{velocidade = (-1.5,vy)}) : aleatFantAndar (s+5) tp j{inimigos = t}
      | alteraImagem3 (realToFrac tp) && (head(geraAleatorios s 1)) < 0 && tipo h == Fantasma && not esc && colisoesParede m h = (h{velocidade = (1.5,vy)}) : aleatFantAndar (s+5) tp j{inimigos = t}
      | otherwise = h : aleatFantAndar (s+1) tp j{inimigos = t}
  where (vx,vy) = velocidade h
      
alteraImagem3 :: Float -> Bool
alteraImagem3 n = alteraImagem3Aux (mod' n 10)

alteraImagem3Aux :: Float -> Bool
alteraImagem3Aux n = (n >= 0 && n<2)

movimentoMacaco :: Tempo -> Jogo -> Jogo
movimentoMacaco t j = if posI == ((14,16.5),Este) && posF == (14,1.5) then j {inimigos = movimentoMacacoAux t (inimigos j)}
                        else j
  where Mapa posI posF blocos = mapa j

movimentoMacacoAux :: Tempo -> [Personagem] -> [Personagem]
movimentoMacacoAux _ [] = []
movimentoMacacoAux tmp (h:t) | tipo h == MacacoMalvado && tmp > 3 && vx == 0 = h{velocidade = (3,0)} : t
                             | tipo h == MacacoMalvado && x > 18.5 = h{velocidade = (-vx,vy)} : t
                             | tipo h == MacacoMalvado && x < 9.5 = h{velocidade = (-vx,vy)} : t
                             | otherwise = h : movimentoMacacoAux tmp t
  where (vx,vy) = velocidade h
        (x,y) = posicao h

ressaltoFantasma :: Jogo -> Jogo 
ressaltoFantasma j = j {inimigos = map (ressaltaFantAux blocos) (inimigos j)}
  where Mapa a b blocos = mapa j

ressaltaFantAux :: [[Bloco]] -> Personagem -> Personagem
ressaltaFantAux blocos inim | procuraBlocoInf blocos (x+0.55,y) == Vazio && tipo inim == Fantasma= inim {velocidade = (-vx,vy)}
                            | procuraBlocoInf blocos (x-0.55,y) == Vazio && tipo inim == Fantasma = inim {velocidade = (-vx,vy)}
                            | (x >= 27.5 || x <= 0.5) && tipo inim == Fantasma = inim {velocidade = (-vx,vy)}
                            | otherwise = inim
  where (vx,vy) = velocidade inim
        (x,y) = posicao inim


