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
{-
movimenta1' :: Jogo -> Jogo
movimenta1' (Jogo {inimigos = ((Personagem {posicao = (xInimigo,yInimigo)
                                          ,tamanho = (lInimigo,aInimigo)
                                          ,vida = vid}) :t )
                  ,jogador = (Personagem {posicao = (xJogador, yJogador)
                                         ,direcao = dir
                                         ,tamanho = (lPersonagem, aPersonagem)
                                         ,aplicaDano = (aplica, tempo)
                                         }
                             )
                   }
             ) 
    | aplica == True = if colisaoHitbox (hitbox (Personagem {posicao = (xInimigo,yInimigo)
                                                            ,tamanho = (lInimigo,aInimigo)
                                                            ,vida = vid})) 
                                        (hitboxDano (Personagem {posicao = (xJogador, yJogador)
                                                                ,direcao = dir
                                                                ,tamanho = (lPersonagem, aPersonagem)
                                                                ,aplicaDano = (aplica, tempo)}))
                          then (Jogo {inimigos = (Personagem {posicao = (xInimigo,yInimigo)
                                                             ,tamanho = (lInimigo,aInimigo)
                                                             ,vida = vid-1})
                                     ,jogador = (Personagem {posicao = (xJogador, yJogador)
                                                            ,direcao = dir
                                                            ,tamanho = (lPersonagem, aPersonagem)
                                                            ,aplicaDano = (aplica, tempo)})}) 
                         else 
    | otherwise = (Jogo {inimigos = ((Personagem {posicao = (xInimigo,yInimigo)
                                          ,tamanho = (lInimigo,aInimigo)
                                          ,vida = vid}) :t )
                        ,jogador = (Personagem {posicao = (xJogador, yJogador)
                                               ,direcao = dir
                                               ,tamanho = (lPersonagem, aPersonagem)
                                               ,aplicaDano = (aplica, tempo)
                                               }
                                   )
                        }
                   ) 
-}
função :: [Personagem] -> Personagem -> [Personagem]
função [] _ = []
função ((Personagem {posicao = (xInimigo,yInimigo)
                    ,tamanho = (lInimigo,aInimigo)
                    ,vida = vid}) :t) 
        (Personagem {posicao = (xJogador, yJogador)
                    ,direcao = dir
                    ,tamanho = (lPersonagem, aPersonagem)
                    ,aplicaDano = (aplica, tempo)
                    }
       )
    | colisaoHitbox (hitbox (Personagem {posicao = (xInimigo,yInimigo)
                                        ,tamanho = (lInimigo,aInimigo)
                                        ,vida = vid
                                        }
                            )
                    )
                    (hitboxDano (Personagem {posicao = (xJogador, yJogador)
                                            ,direcao = dir
                                            ,tamanho = (lPersonagem, aPersonagem)
                                            ,aplicaDano = (aplica, tempo)
                                            }
                                )
                    )
                    = (Personagem {posicao = (xInimigo,yInimigo)
                                  ,tamanho = (lInimigo,aInimigo)
                                  ,vida = vid-1}) : função t (Personagem {posicao = (xJogador, yJogador)
                                                                       ,direcao = dir
                                                                       ,tamanho = (lPersonagem, aPersonagem)
                                                                       ,aplicaDano = (aplica, tempo)
                                                                       }
                                                            )
    | otherwise = (Personagem {posicao = (xInimigo,yInimigo)
                              ,tamanho = (lInimigo,aInimigo)
                              ,vida = vid-1}) : função t (Personagem {posicao = (xJogador, yJogador)
                                                                     ,direcao = dir
                                                                     ,tamanho = (lPersonagem, aPersonagem)
                                                                     ,aplicaDano = (aplica, tempo)
                                                                     }
                                                         )
        
{-
movimentaaa :: Jogo -> Jogo
movimentaaa (Jogo {inimigos = ((Personagem {vida = vidaInimigo}) : t)
                  ,jogador = (Personagem {aplicaDano = (aplica, tempo)})
                  }
            ) 
    | aplica == True && tempo /= 0 = if colisaoHitbox (hitbox (Personagem {vida = vidaInimigo})) (hitboxDano (Personagem {aplicaDano = (aplica, tempo)}))
                                       then movimentaaa (Jogo {inimigos = ((Personagem {vida = vidaInimigo-1}) : t)
                                                              ,jogador = (Personagem {aplicaDano = (aplica, tempo)})
                                                              })
                                       else movimentaaa (Jogo {inimigos = ((Personagem {vida = vidaInimigo}) : t)
                  ,jogador = (Personagem {aplicaDano = (aplica, tempo)})
                  })
    | otherwise = movimentaaa (Jogo {inimigos = ((Personagem {vida = vidaInimigo}) : t)
                  ,jogador = (Personagem {aplicaDano = (aplica, tempo)})
                  })
-}
{-|

Varifica se duas hitbox estão em colisão

=Exemplos
>>>colisaoHitbox ((1,4),(3,1)) ((2,5),(4,3)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((2,2),(4,0)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((0,2),(4,0)) = True
>>>colisaoHitbox ((1,4),(3,1)) ((4,2),(5,0)) =False
-}
colisaoHitbox :: Hitbox -> Hitbox -> Bool
colisaoHitbox ((x1,y1),(x2,y2)) ((x3,y3),(x4,y4)) | x1 >= x3 && x1 <= x4 && y1 <= y3 && y1 >= y4 = True -- ponto inferior esquerdo
                                                  | x2 >= x3 && x2 <= x4 && y2 <= y3 && y2 >= y4 = True -- ponto superior direito
                                                  | x1 >= x3 && x1 <= x4 && y2 <= y3 && y2 >= y4 = True -- ponto superior esquerdo
                                                  | x2 >= x3 && x2 <= x4 && y1 <= y3 && y1 >= y4 = True -- ponto inferior direito
                                                  | otherwise = False





{-
Um inimigo perde 1 (uma) vida se estiver dentro da hitbox de dano de
um jogador armado. Por jogador armado entende-se um jogador cuja
componente aplicaDano esteja activa e com tempo restante. Note
que a hitbox de dano n˜ao ´e a mesma hitbox do jogador, mas antes uma
hitbox com as dimens˜oes do jogador posicionada exactamente `a frente
do jogador, cf. 

-}