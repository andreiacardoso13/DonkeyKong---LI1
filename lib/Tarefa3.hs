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
import Mapa

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta s t j = atualizaDirecao $ tempoAplicaDano t $ efeitoColisoes $ removeAlcapao $ recolheColecionavel $ ataqueDoInimigo $ efeitoGravidade $ ataqueDoJogador j
      
{-|
Se o jogador tiver a componente aplicaDano activa e com tempo restante e a 
hitbox de dano do jogador colidir com um fantasma retira uma vida ao fantasma

=Exemplos

>>>ataqueDoJogador (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]]
                    ,inimigos = [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1.5,1.5),direcao = Oeste, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}
                                ,Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (4,1),direcao = Oeste, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}]
                    ,colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))]
                    ,jogador = Personagem {velocidade = (1,2),tipo = Jogador,posicao = (2.5,1),direcao = Oeste, tamanho = (1,2),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
               =Jogo {mapa = Mapa ((2.5,1.0),Norte) (2.5,1.0) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]]
                     ,inimigos = [Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (1.5,1.5), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 0, pontos = 0, aplicaDano = (False,0.0)}
                                 ,Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (4.0,1.0), direcao = Oeste, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
                     ,colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))]
                     ,jogador = Personagem {velocidade = (1.0,2.0), tipo = Jogador, posicao = (2.5,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5.0)}}
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
>>>ataqueDoJogadorInim [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (7,7),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}
                 ,Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}] 
                 (Personagem {posicao = (0.5,1),direcao = Este,tamanho = (1,2),aplicaDano = (True,5)})
                 =
                [Personagem {velocidade = (1.0,2.0),tipo = Fantasma,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}
                ,Personagem {velocidade = (1.0,2.0),tipo = Fantasma,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}]
-}
ataqueDoJogadorInim :: [Personagem] -> Personagem -> [Personagem]
ataqueDoJogadorInim [] _ = []
ataqueDoJogadorInim (inim :t) jog | colisaoHitbox (hitbox inim) (hitboxDano jog) && (tipo inim) == Fantasma = (inim {vida = vida inim - 1}) : ataqueDoJogadorInim t jog
                                  | otherwise = inim : ataqueDoJogadorInim t jog
        
{-| 
Se a hitbox de dano do jogador colidir com um inimigo adiciona 500 pontos ao jogador

=Exemplo
>>>ataqueDoJogadorJog [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}
                  ,Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}] 
                  (Personagem {velocidade = (1,0),tipo = Jogador,posicao = (0.5,1),direcao = Este,tamanho = (1,2),emEscada = False,ressalta = False,vida=3,pontos=0,aplicaDano = (True,5)})
                  =
                   Personagem {velocidade = (1.0,0.0), tipo = Jogador, posicao = (0.5,1.0), direcao = Este, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 1000, aplicaDano = (True,5.0)}
-}
ataqueDoJogadorJog :: [Personagem] -> Personagem -> Personagem
ataqueDoJogadorJog [] jog = jog
ataqueDoJogadorJog (inim :t) jog | colisaoHitbox (hitbox inim) (hitboxDano jog) && (tipo inim) == Fantasma && vida inim > 0 = ataqueDoJogadorJog t (jog {pontos = pontos jog + 500})
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
efeitoGravidadeInimigos m (inim : t) | procuraBlocoInf m (posicao inim) == Vazio = inim {velocidade = (fst (velocidade inim), snd gravidade)} : efeitoGravidadeInimigos m t
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
>>>ataqueDoInimigo (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]]
                    ,inimigos = [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Oeste, tamanho = (2,2),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}]
                    ,colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))]
                    ,jogador = Personagem {velocidade = (1,2),tipo = Jogador,posicao = (2,2),direcao = Oeste, tamanho = (2,2),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
              =Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))], jogador = Personagem {velocidade = (1.0,2.0), tipo = Jogador, posicao = (2.0,2.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = False, vida = 2, pontos = 0, aplicaDano = (True,5.0)}}
-}
ataqueDoInimigo :: Jogo -> Jogo
ataqueDoInimigo (Jogo {mapa = m 
                      ,inimigos = listaInimigos
                      ,colecionaveis = listaColecionaveis
                      ,jogador = jog}) = (Jogo {mapa = m 
                                               ,inimigos = listaInimigos
                                               ,colecionaveis = listaColecionaveis
                                               ,jogador = ataqueDoInimigoAux listaInimigos jog})

{-|
Se o jogador colidir com um inimigo retira-lhe uma vida e altera a sua posição para a inicial

=Exemplos
>>> ataqueDoInimigoAux [Personagem {posicao = (1,1),tamanho = (2,2),vida = 1}] 
                  (Personagem {velocidade = (1,2),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos= 0,aplicaDano = (False,0)})
                   = 
                   Personagem {velocidade = (1.0,2.0),tipo = Jogador,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}
-}
ataqueDoInimigoAux :: [Personagem] -> Personagem -> Personagem 
ataqueDoInimigoAux [] p = p
ataqueDoInimigoAux (inim : t) jog | colisoesPersonagens inim jog && (vida inim > 0 )= (jog {posicao = (0.5,16.5), vida = vida jog -1})
                                  | otherwise = ataqueDoInimigoAux t jog

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


efeitoColisoes :: Jogo -> Jogo
efeitoColisoes jogo' = jogo' {inimigos = efeitoColisoesInimigos (mapa jogo') (inimigos jogo'), jogador = efeitoColisoesJogador (mapa jogo') (jogador jogo')}



efeitoColisoesJogador :: Mapa -> Personagem -> Personagem
efeitoColisoesJogador m jog = efeitoColisoesPlataforma m (efeitoColisoesMapa m jog)

efeitoColisoesInimigos :: Mapa -> [Personagem] -> [Personagem]
efeitoColisoesInimigos _ [] = []
efeitoColisoesInimigos m (h:t) = map (efeitoColisoesPlataforma m) (map (efeitoColisoesMapa m) (h:t))

efeitoColisoesPlataforma :: Mapa -> Personagem -> Personagem
efeitoColisoesPlataforma (Mapa a b blocos) pers | platColisoes (Mapa a b blocos) pers && procuraBlocoInf blocos (posicao pers) == Plataforma = pers {velocidade = (fst (velocidade pers),0)}
                                                   | platColisoes (Mapa a b blocos) pers && procuraBlocoSup blocos (posicao pers) == Plataforma = pers {velocidade = (fst (velocidade pers,-(snd(velocidade pers))))}
                                                   | platColisoes (Mapa a b blocos) pers = pers {velocidade = (0,snd (velocidade pers))}
                                                   | otherwise = pers

efeitoColisoesMapa :: Mapa -> Personagem -> Personagem
efeitoColisoesMapa m jog | mapaLimites m jog = if fst(posicao jog) > 14 
                                                    then jog {posicao = (fst (posicao jog) -0.1, snd (posicao jog))}
                                                    else jog {posicao = (fst (posicao jog) +0.1, snd (posicao jog))}
                         | otherwise = jog

-- faz o efeito do tempo no parâmetro aplicaDano do jogador

tempoAplicaDano :: Tempo -> Jogo -> Jogo
tempoAplicaDano t (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog}) | snd (aplicaDano jog) <= 0 = (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog {aplicaDano = (False, 0)}})
                                                                                         | snd (aplicaDano jog) > 0 = (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog {aplicaDano = (True, snd (aplicaDano jog) - t)}})
                                                                                         | otherwise = (Jogo {mapa = m, inimigos = inim, colecionaveis = col, jogador = jog})

atualizaDirecao :: Jogo -> Jogo 
atualizaDirecao jog = jog {jogador = atualizaDirecaoPersonagem (jogador jog), inimigos = atualizaDirecaoInim (inimigos jog)}

atualizaDirecaoPersonagem :: Personagem -> Personagem
atualizaDirecaoPersonagem pers | abs x > abs y && x > 0 = pers {direcao = Este}
                               | abs x > abs y && x < 0 = pers {direcao = Oeste}
                               | abs x < abs y && y > 0 = pers {direcao = Norte}
                               | abs x < abs y && y < 0 = pers {direcao = Sul}
                               | otherwise = pers
  where x = fst (velocidade pers)
        y = snd (velocidade pers)

atualizaDirecaoInim :: [Personagem] -> [Personagem]
atualizaDirecaoInim [] = []
atualizaDirecaoInim (h:t) = (atualizaDirecaoPersonagem h) : atualizaDirecaoInim t



{-
NOTAS 

utilizar a tarefa4 para se o tempo passar de 5 macaco começar a andar. E se chegar a certa posição ir para a esquerda

fazer lista com os tamanhos de todas as entidades

fazer com que o bonus seja dado ao jogador quando este ganha

fazer uma hitbox especifica para quando o Mario está com o Martelo, e definir o seu tamanho apenas como o sitio onde tá o Mario

Main: Tarefa2.hs:(410,1)-(412,45): Non-exhaustive patterns in function procuraBloco
aconteceu isto quando estava numa plataforma e cliquei seta para baixo

o jogo acaba quando a vidad do jogador é igual a 0 ou quando este chega à estrela

em relação ao movimento aleatorio do inimigo, ele poderia fazer essa aleatoriadade pelo tempo, por exemplo se o tempo do estado for multiplo de 5 faz essa escolha...
mas continuar a fazer essa aleatoriedade sempre que passa por uma escada e pode subir/descer

-}

--j1 :: Jogo
--j1 = 
   --   (Jogo {mapa = mapaInicial,inimigos = [Personagem {velocidade=(1,0),tipo=Fantasma,posicao=(5.5,4.5),direcao = Este,tamanho = (1,1),emEscada=False,ressalta=True,vida=1,pontos=0,aplicaDano=(False,0)},Personagem {velocidade=(1,0),tipo=Fantasma,posicao=(8.5,7.5),direcao = Norte,tamanho = (1,1),emEscada=False,ressalta=True,vida=1,pontos=0,aplicaDano=(False,0)}],colecionaveis = [(Moeda,(17.5,7.5)),(Martelo, (8.5,10.5))],jogador = Personagem {velocidade=(1,0),tipo=Jogador,posicao=(0.5,16.5),direcao = Este,tamanho = (1,1),emEscada=False,ressalta= False,vida=3,pontos=0,aplicaDano=(False,0)}})
   