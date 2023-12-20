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

movimenta :: Semente -> Tempo -> Jogo -> Jogo
movimenta = undefined

{-|

Se o jogador tiver a componente aplicaDano activa e com tempo restante e a 
hitbox de dano do jogador colidir com um inimigo retira uma vida ao inimigo

=Exemplos

>>>movimenta1 (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]]
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


movimenta1 :: Jogo -> Jogo
movimenta1 (Jogo {mapa = m ,inimigos = listaInimigos,colecionaveis = listaColecionaveis,jogador = jog}) 
        | fst (aplicaDano jog) == True && snd (aplicaDano jog) > 0 = Jogo {mapa = m 
                                                                          ,inimigos = ataqueJogador listaInimigos jog
                                                                          ,colecionaveis = listaColecionaveis
                                                                          ,jogador = jog
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
>>>ataqueJogador [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (7,7),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}
                 ,Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos = 0,aplicaDano = (False,0)}] 
                 (Personagem {posicao = (0.5,1),direcao = Este,tamanho = (1,2),aplicaDano = (True,5)})
                 =
                [Personagem {velocidade = (1.0,2.0),tipo = Fantasma,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}
                ,Personagem {velocidade = (1.0,2.0),tipo = Fantasma,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}]
-}

ataqueJogador :: [Personagem] -> Personagem -> [Personagem]
ataqueJogador [] _ = []
ataqueJogador (inim :t) jog | colisaoHitbox (hitbox inim) (hitboxDano jog) && (tipo inim) == Fantasma = (inim {vida = vida inim - 1}) : ataqueJogador t jog
                            | otherwise = inim : ataqueJogador t jog
        

{-|

Se os persongens estiverem em cima dum bloco "Vazio"
altera a sua velocidade para ser igual à gravidade

=Exemplos
>>>movimenta3 (Jogo {mapa = Mapa ((2.5 , 1),Oeste) (2.5,1) [[Vazio,Vazio], [Vazio,Plataforma], [Vazio,Vazio], [Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1,0), tipo = Fantasma,posicao = (1.5,0.5),direcao = Este, tamanho = (1,1),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}],colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))],jogador = Personagem {velocidade = (1,0),tipo = Jogador,posicao = (0.5,0.5),direcao = Este, tamanho = (1,1),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
              =Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))], jogador = Personagem {velocidade = (0.0,10.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5.0)}}

-}

movimenta3 :: Jogo -> Jogo
movimenta3 (Jogo {mapa = (Mapa posI posF matriz) 
                 ,inimigos = listaInimigos
                 ,colecionaveis = listaColecionaveis
                 ,jogador = jog}) = (Jogo {mapa = (Mapa posI posF matriz)
                                          ,inimigos = movimenta3Inimigos matriz listaInimigos
                                          ,colecionaveis = listaColecionaveis
                                          ,jogador = movimenta3Jogador matriz jog})

{-|
Verifica onde estão os inimigos e se estiverem em cima do bloco 
"Vazio" altera a sua velocidade para ser igual à gravidade

=Exemplos
>>> movimenta3Inimigos [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]] [Personagem {velocidade = (0,2),tipo = Fantasma, posicao = (1.5,0.5),direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
                      =[Personagem {velocidade = (0.0,2.0), tipo = Fantasma, posicao = (1.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
>>> movimenta3Inimigos [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]] [Personagem {velocidade = (0,2),tipo = Fantasma, posicao = (0.5,0.5),direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
                      =[Personagem {velocidade = (0.0,10.0), tipo = Fantasma, posicao = (0.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}]
-}

movimenta3Inimigos :: [[Bloco]] -> [Personagem] -> [Personagem]
movimenta3Inimigos m [] = []
movimenta3Inimigos m (inim : t) | procuraBlocoInf m (posicao inim) == Vazio = inim {velocidade = gravidade} : movimenta3Inimigos m t
                                | otherwise = inim : movimenta3Inimigos m t

{-|
Verifica onde estão o jogador e se estiver em cima do bloco 
"Vazio" altera a sua velocidade para ser igual à gravidade

=Exemplos
>>> movimenta3Jogador [[Vazio,Vazio],[Vazio,Plataforma],[Vazio,Vazio],[Plataforma,Plataforma]] (Personagem {velocidade = (2,0),tipo = Jogador, posicao = (0.5,0.5),direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)})
                     = Personagem {velocidade = (0.0,10.0), tipo = Jogador, posicao = (0.5,0.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}
-}

movimenta3Jogador :: [[Bloco]] -> Personagem -> Personagem
movimenta3Jogador m jog | procuraBlocoInf m (posicao jog) == Vazio = jog {velocidade = gravidade}

{-|
Se o jogador colidir com o inimigo retira uma vida ao jogador

=Exemplos
>>>movimenta4 (Jogo {mapa = Mapa ((2.5,1),Oeste) (2.5,1) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]]
                    ,inimigos = [Personagem {velocidade = (1,2),tipo = Fantasma,posicao = (1,1),direcao = Oeste, tamanho = (2,2),emEscada = False,ressalta = True,vida = 1, pontos = 0, aplicaDano = (False,0)}]
                    ,colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))]
                    ,jogador = Personagem {velocidade = (1,2),tipo = Jogador,posicao = (2,2),direcao = Oeste, tamanho = (2,2),emEscada = False,ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,5)}})
              =Jogo {mapa = Mapa ((2.5,1.0),Oeste) (2.5,1.0) [[Escada,Alcapao,Vazio],[Escada,Vazio,Plataforma],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,2.0), tipo = Fantasma, posicao = (1.0,1.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(1.0,1.0)),(Moeda,(2.0,0.0))], jogador = Personagem {velocidade = (1.0,2.0), tipo = Jogador, posicao = (2.0,2.0), direcao = Oeste, tamanho = (2.0,2.0), emEscada = False, ressalta = False, vida = 2, pontos = 0, aplicaDano = (True,5.0)}}
-}


movimenta4 :: Jogo -> Jogo
movimenta4 (Jogo {mapa = m 
                 ,inimigos = listaInimigos
                 ,colecionaveis = listaColecionaveis
                 ,jogador = jog}) = (Jogo {mapa = m 
                                          ,inimigos = listaInimigos
                                          ,colecionaveis = listaColecionaveis
                                          ,jogador = ataqueInimigo listaInimigos jog})

{-|
Se o jogador colidir com um inimigo retira uma vida ao jogador

=Exemplos
>>> ataqueInimigo [Personagem {posicao = (1,1),tamanho = (2,2),vida = 1}] 
                  (Personagem {velocidade = (1,2),tipo = Jogador,posicao = (1,1),direcao = Este,tamanho = (2,2),emEscada = False,ressalta = True,vida = 1,pontos= 0,aplicaDano = (False,0)})
                   = 
                   Personagem {velocidade = (1.0,2.0),tipo = Jogador,posicao = (1.0,1.0),direcao = Este,tamanho = (2.0,2.0),emEscada = False,ressalta = True,vida = 0,pontos = 0,aplicaDano = (False,0.0)}
-}

ataqueInimigo :: [Personagem] -> Personagem -> Personagem 
ataqueInimigo [] p = p
ataqueInimigo (inim : t) jog | colisoesPersonagens inim jog && (vida inim > 0 )= (jog {vida = vida jog -1})
                             | otherwise = ataqueInimigo t jog

{-|
Reflete as consequências de quando um colecionável é recolhido

=Exemplos
>>>movimenta5 (Jogo {mapa= Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1,0),tipo = Fantasma,posicao=(0.5,1.5),direcao =Este,tamanho = (1,1),emEscada = False,ressalta=True,vida = 1, pontos = 0, aplicaDano = (False,0)}], colecionaveis = [(Moeda,(1.5,1.5)),(Martelo,(0.5,1.5))], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
              =Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(0.5,1.5))], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 200, aplicaDano = (False,0.0)}}
>>>movimenta5 (Jogo {mapa= Mapa ((2,1),Oeste) (2,1) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1,0),tipo = Fantasma,posicao=(0.5,1.5),direcao =Este,tamanho = (1,1),emEscada = False,ressalta=True,vida = 1, pontos = 0, aplicaDano = (False,0)}], colecionaveis = [(Moeda,(0.5,1.5)),(Martelo,(1.5,1.5))], jogador = Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)}})
              =Jogo {mapa = Mapa ((2.0,1.0),Oeste) (2.0,1.0) [[Vazio,Vazio,Vazio],[Vazio,Vazio,Vazio],[Plataforma,Plataforma,Plataforma]], inimigos = [Personagem {velocidade = (1.0,0.0), tipo = Fantasma, posicao = (0.5,1.5), direcao = Este, tamanho = (1.0,1.0), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0.0)}], colecionaveis = [(Martelo,(0.5,1.5))], jogador = Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,10.0)}}
-}
movimenta5 :: Jogo -> Jogo 
movimenta5 (Jogo {mapa = m ,inimigos = listaInimigos,colecionaveis = listaColecionaveis,jogador = jog}) = Jogo {mapa = m 
                                                                                                               ,inimigos = listaInimigos
                                                                                                               ,colecionaveis = movimenta51 listaColecionaveis jog
                                                                                                               ,jogador = movimenta52 listaColecionaveis jog}
{-|
Se houver colisão entre um personagem e um colecionável remove o colécionável da lista

=Exemplos
>>> movimenta51 [(Moeda,(1.5,1.5)),(Martelo,(0.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
               =[(Martelo,(0.5,1.5))]
>>> movimenta51 [(Moeda,(0.5,1.5)),(Martelo,(1.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
               =[(Moeda,(0.5,1,5))]
-}
movimenta51 :: [(Colecionavel, Posicao)] -> Personagem -> [(Colecionavel, Posicao)]
movimenta51 [] jog = []
movimenta51 (h:t) jog | colisaoHitbox (hitboxColecionavel (snd h)) (hitbox jog) = movimenta51 t jog
                      | otherwise = h : movimenta51 t jog
{-|
Devolve o personagem com as consequência de recolher um colecionável
(apenas de este o recolher)

=Exemplos
>>> movimenta52 [(Moeda,(1.5,1.5)),(Martelo,(0.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
               =Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 200, aplicaDano = (False,0.0)}
>>> movimenta52 [(Moeda,(0.5,1.5)),(Martelo,(1.5,1.5))] (Personagem {velocidade = ((-1),0), tipo = Jogador, posicao = (2,1),direcao=Oeste,tamanho=(1,2),emEscada=False,ressalta=False,vida=3,pontos=0,aplicaDano=(False,0)})
               =Personagem {velocidade = (-1.0,0.0), tipo = Jogador, posicao = (2.0,1.0), direcao = Oeste, tamanho = (1.0,2.0), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (True,10.0)}
-}
movimenta52 :: [(Colecionavel, Posicao)] -> Personagem -> Personagem
movimenta52 [] jog = jog
movimenta52 (h:t) jog | colisaoHitbox (hitboxColecionavel (snd h)) (hitbox jog) = if fst h == Moeda
                                                                                    then movimenta52 t (jog {pontos = pontos jog + 200})
                                                                                    else movimenta52 t (jog {aplicaDano = (True,10)})
                      | otherwise = movimenta52 t jog



{-
NOTAS 

Fazer função que atualiza direção automaticamente consoante a velocidade

adicionar pontos ao jogador quando este mata o fantasma

fazer o fantasma desaparecer quando sobre ataqueJogador

o jogo acaba quando a vidad do jogador é igual a 0 ou quando este chega à estrela

-}
