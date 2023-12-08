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

Define a área onde um personagem consegue causar dano,
sendo esta no meu tamanho do menor retângulo que contém um personagem

=Exemplos
>>> hitboxDano (Personagem {posicao = (1,1), tamanho = (1,1), direcao = Este}) = ((1.5 , 0.5) , (2.5 , 1.5))
>>> hitboxDano (Personagem {posicao = (1,1), tamanho = (1,1), direcao = Oeste}) = ((-0.5 , 0.5) , (0.5 , 1.5))
-}

hitboxDano :: Personagem -> Hitbox
hitboxDano (Personagem {posicao = (x,y), tamanho = (l,a), direcao = dir}) | dir == Oeste = ((x-(3*l/2),y-(a/2)),(x - (l/2), y + (a/2)))
                                                                          | dir == Este = ((x + (l/2),y-(a/2)),(x + (3*l/2),y + (a/2)))
{-|
Se a hitbox de dano do jogador colidir com um inimigo retira uma vida ao mesmo

=Exemplos
>>>ataqueJogador [Personagem {velocidade = (1,2)
                             ,tipo = Fantasma
                             ,posicao = (7,7)
                             ,direcao = Este
                             ,tamanho = (2,2)
                             ,emEscada = False
                             ,ressalta = True
                             ,vida = 1,pontos = 0
                             ,aplicaDano = (False,0)}
                 ,Personagem {velocidade = (1,2)
                             ,tipo = Fantasma
                             ,posicao = (1,1)
                             ,direcao = Este
                             ,tamanho = (2,2)
                             ,emEscada = False
                             ,ressalta = True
                             ,vida = 1,pontos = 0
                             ,aplicaDano = (False,0)
                             }
                 ] 
                 (Personagem {posicao = (0.5,1)
                             ,direcao = Este
                             ,tamanho = (1,2)
                             ,aplicaDano = (True,5)})
                 =
                [Personagem {velocidade = (1.0,2.0)
                            ,tipo = Fantasma
                            ,posicao = (1.0,1.0)
                            ,direcao = Este
                            ,tamanho = (2.0,2.0)
                            ,emEscada = False
                            ,ressalta = True
                            ,vida = 0
                            ,pontos = 0
                            ,aplicaDano = (False,0.0)}
                ,Personagem {velocidade = (1.0,2.0)
                            ,tipo = Fantasma
                            ,posicao = (1.0,1.0)
                            ,direcao = Este
                            ,tamanho = (2.0,2.0)
                            ,emEscada = False
                            ,ressalta = True
                            ,vida = 0
                            ,pontos = 0
                            ,aplicaDano = (False,0.0)}]
-}

ataqueJogador :: [Personagem] -> Personagem -> [Personagem] -- terá de ser usada numa futura função que recebe um jogo e verifica se a aplicaDano do Jogador tá ativa
ataqueJogador [] _ = []
ataqueJogador ((Personagem {velocidade = vI
                           ,tipo = entI
                           ,posicao = (xInimigo,yInimigo)
                           ,direcao = dirI
                           ,tamanho = (lInimigo,aInimigo)
                           ,emEscada = escI
                           ,ressalta = resI
                           ,vida = vidI
                           ,pontos = pI
                           ,aplicaDano = aplicaI
                           }
               ) :t) 
              (Personagem {posicao = (xJogador, yJogador)
                          ,direcao = dirJ
                          ,tamanho = (lJogador, aJogador)
                          ,aplicaDano = (aplicaJ, tempo)
                          }
              )
    | colisaoHitbox (hitbox (Personagem {velocidade = vI
                                        ,tipo = entI
                                        ,posicao = (xInimigo,yInimigo)
                                        ,direcao = dirI
                                        ,tamanho = (lInimigo,aInimigo)
                                        ,emEscada = escI
                                        ,ressalta = resI
                                        ,vida = vidI
                                        ,pontos = pI
                                        ,aplicaDano = aplicaI
                                         }
                            )
                    )
                    (hitboxDano (Personagem {posicao = (xJogador, yJogador)
                                            ,direcao = dirJ
                                            ,tamanho = (lJogador, aJogador)
                                            ,aplicaDano = (aplicaJ, tempo)
                                            }
                                )
                    )
                    = (Personagem {velocidade = vI
                                  ,tipo = entI
                                  ,posicao = (xInimigo,yInimigo)
                                  ,direcao = dirI
                                  ,tamanho = (lInimigo,aInimigo)
                                  ,emEscada = escI
                                  ,ressalta = resI
                                  ,vida = vidI - 1
                                  ,pontos = pI
                                  ,aplicaDano = aplicaI
                                   }
                      ) : ataqueJogador t (Personagem {posicao = (xJogador, yJogador)
                                                      ,direcao = dirJ
                                                      ,tamanho = (lJogador, aJogador)
                                                      ,aplicaDano = (aplicaJ, tempo)
                                                      }
                                          )
    | otherwise = (Personagem {velocidade = vI
                              ,tipo = entI
                              ,posicao = (xInimigo,yInimigo)
                              ,direcao = dirI
                              ,tamanho = (lInimigo,aInimigo)
                              ,emEscada = escI
                              ,ressalta = resI
                              ,vida = vidI
                              ,pontos = pI
                              ,aplicaDano = aplicaI
                              }
                   ) : ataqueJogador t (Personagem {posicao = (xJogador, yJogador)
                                                   ,direcao = dirJ
                                                   ,tamanho = (lJogador, aJogador)
                                                   ,aplicaDano = (aplicaJ, tempo)
                                                   }
                                       )
        

{-|

Varifica se duas hitbox estão em colisão

=Exemplos
>>>colisaoHitbox ((1,4),(3,1)) ((2,5),(4,3)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((2,2),(4,0)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((0,2),(4,0)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((4,2),(5,0)) =False
-}
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) | x1 >= x3 && x1 <= x4 && y2 >= y3 && y2 <= y4 = True -- ponto inferior esquerdo
                                                  | x2 >= x3 && x2 <= x4 && y1 >= y3 && y1 <= y4 = True -- ponto superior direito
                                                  | x1 >= x3 && x1 <= x4 && y1 >= y3 && y1 <= y4 = True -- ponto superior esquerdo
                                                  | x2 >= x3 && x2 <= x4 && y2 >= y3 && y2 <= y4 = True -- ponto inferior direito
                                                  | otherwise = False


ataqueInimigo :: [Personagem] -> Personagem -> Personagem -- ainda nao testada
ataqueInimigo [] p = p
ataqueInimigo ((Personagem {posicao = (xInimigo,yInimigo)
                           ,direcao = dirI
                           ,tamanho = (lInimigo,aInimigo)
                           }
               ) :t) 
              (Personagem {velocidade = vJ
                          ,tipo = entJ
                          ,posicao = (xJOgador,yJogador)
                          ,direcao = dirJ
                          ,tamanho = (lJogador,aJogador)
                          ,emEscada = escJ
                          ,ressalta = resJ
                          ,vida = vidJ
                          ,pontos = pJ
                         ,aplicaDano = (aplicaJ, tempo)
                          }
              )
    | colisaoHitbox (hitbox (Personagem {posicao = (xInimigo,yInimigo)
                                        ,direcao = dirI
                                        ,tamanho = (lInimigo,aInimigo)
                                        }
                            )
                    )
                    (hitbox (Personagem {velocidade = vJ
                                            ,tipo = entJ
                                            ,posicao = (xJOgador,yJogador)
                                            ,direcao = dirJ
                                            ,tamanho = (lJogador,aJogador)
                                            ,emEscada = escJ
                                            ,ressalta = resJ
                                            ,vida = vidJ
                                            ,pontos = pJ
                                            ,aplicaDano = (aplicaJ, tempo)
                                            }
                                )
                    )
                    = (Personagem {velocidade = vJ
                                  ,tipo = entJ
                                  ,posicao = (xJOgador,yJogador)
                                  ,direcao = dirJ
                                  ,tamanho = (lJogador,aJogador)
                                  ,emEscada = escJ
                                  ,ressalta = resJ
                                  ,vida = vidJ - 1 
                                  ,pontos = pJ
                                  ,aplicaDano = (aplicaJ, tempo)
                                   }
                      )
    | otherwise = ataqueInimigo t (Personagem {velocidade = vJ
                                              ,tipo = entJ
                                              ,posicao = (xJOgador,yJogador)
                                              ,direcao = dirJ
                                              ,tamanho = (lJogador,aJogador)
                                              ,emEscada = escJ
                                              ,ressalta = resJ
                                              ,vida = vidJ
                                              ,pontos = pJ
                                              ,aplicaDano = (aplicaJ, tempo)
                                              }
                                   )