{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324
import Tarefa1

{-|

Recebe um jogo e verifica se este cumpre todas as condições necessárias para ser válido

=Exemplos
>>>valida (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Escada    ,Alcapao   ,Vazio],
                                                  [Escada    ,Vazio     ,Plataforma],
                                                  [Plataforma,Plataforma,Plataforma]]
                ,inimigos = [Personagem {tipo = Fantasma,posicao = (1,1),ressalta = True,vida = 1}
                            ,Personagem {tipo = Fantasma,posicao = (0,0),ressalta = True,vida = 1}]
                ,colecionaveis = [(Martelo, (1,1)),(Moeda, (2,0))]
                ,jogador = Personagem {posicao = (2,0),tamanho = (1,2),ressalta = False}})
                =
                True
-}

valida :: Jogo -> Bool
valida jogo' = validaChao jogo' &&
              validaRessalta jogo' &&
              validaPosicaoColisao jogo' &&
              validaNumInimigos jogo' &&
              validaVidaFantasma jogo' &&
              validaEscadas jogo' &&
              validaLarguraAlcapao jogo' &&
              validaPosicaoMapa jogo'
{-|

Recebe um jogo e verifica se o mapa tem chão

=Exemplos
>>> validaChao (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Vazio     ,Vazio     ,Escada],
                                                       [Plataforma,Plataforma,Plataforma]]})
                                                       =True
>>> validaChao (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Vazio     ,Vazio     ,Escada],
                                                       [Alcapao   ,Plataforma,Plataforma]]})
                                                       =False
>>> validaChao (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Vazio     ,Vazio     ,Escada],
                                                       [Escada    ,Plataforma,Plataforma]]})
                                                       =False
-}
validaChao :: Jogo -> Bool -- verifica se o mapa tem chão
validaChao (Jogo {mapa = Mapa _ _ (h:t)}) = validaChaoAux (last t)


{-|

Recebe uma lista de blocos e verifica se todos os blocos são do tipo Plataforma

=Exemplos
>>> validaChaoAux [Plataforma,Plataforma,Plataforma] = True
>>> validaChaoAux [Plataforma,Vazio,Plataforma] = False
>>> validaChaoAux [Alcapao,Plataforma,Plataforma] = False
>>> validaChaoAux [Plataforma,Plataforma,Escada] = False
-}
validaChaoAux :: [Bloco] -> Bool
validaChaoAux [] = True
validaChaoAux (h:t) | h == Plataforma = validaChaoAux t
                    | otherwise = False

{-|
Verifica se todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.

=Exemplos
>>> validaRessalta (Jogo {inimigos = [Personagem {ressalta = True}
                                     ,Personagem {ressalta = False}]
                         ,jogador = Personagem {ressalta = False}}) 
                         = False
>>> validaRessalta (Jogo {inimigos = [Personagem {ressalta = False}
                                     ,Personagem {ressalta = True}]
                         ,jogador = Personagem {ressalta = False}}) 
                         = False
>>> validaRessalta (Jogo {inimigos = [Personagem {ressalta = True}
                                     ,Personagem {ressalta = True}]
                         ,jogador = Personagem {ressalta = True}}) 
                         = False
>>> validaRessalta (Jogo {inimigos = [Personagem {ressalta = True}
                                     ,Personagem {ressalta = True}]
                          ,jogador = Personagem {ressalta = False}}) 
                          = True
-}

validaRessalta :: Jogo -> Bool
validaRessalta (Jogo {inimigos = [], jogador = (Personagem {ressalta = y})}) = True
validaRessalta (Jogo {inimigos = ((Personagem {ressalta = x}): t ), jogador = (Personagem {ressalta = y})}) 
      | x == True && y == False = validaRessalta (Jogo {inimigos = t, jogador = (Personagem {ressalta = y})}) 
      | otherwise = False

{-|
Recebe um jogo e verifica se a posição do jogador colide com a posição de algum outro personagem

=Exemplos
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (1,2)}),Personagem {posicao = (1,2)}]
                               ,jogador = Personagem {posicao = (1,3)}}) 
                                = True
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (1,3)}),Personagem {posicao = (1,2)}]
                               ,jogador = Personagem {posicao = (1,3)}}) 
                               = False
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (1,2)}),Personagem {posicao = (1,3)}]
                               ,jogador = Personagem {posicao = (1,3)}}) 
                               = False
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (2,4)}),Personagem {posicao = (5,1)}]
                               ,jogador = Personagem {posicao = (3,2)}}) 
                               = True
-}

validaPosicaoColisao :: Jogo -> Bool
validaPosicaoColisao (Jogo {inimigos = [], jogador = Personagem {posicao = (x2,y2)}}) = True
validaPosicaoColisao (Jogo {inimigos = ((Personagem {posicao = (x1,y1)}): t ), jogador = Personagem {posicao = (x2,y2)}}) 
      | x1 == x2 && y1 == y2 = False
      | otherwise = validaPosicaoColisao (Jogo {inimigos = t , jogador = Personagem {posicao = (x2,y2)}}) 

{-|
Recebe um jogo e verifica se o jogo tem pelo menos 2 inimigos

=Exemplos
>>> validaNumInimigos (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 5}),Personagem {posicao = (1,2), vida = 1},Personagem {ressalta = True}]}) = True
>>> validaNumInimigos (Jogo {inimigos = [(Personagem {posicao = (1,2), vida = 1},Personagem {ressalta = True}]}) = True
>>> validaNumInimigos (Jogo {inimigos = [(Personagem {ressalta = True}]}) = False
>>> validaNumInimigos (Jogo {inimigos = []}) = False
-}

validaNumInimigos :: Jogo -> Bool -- ainda não testada // verifica se o jogo tem pelo menos 2 inimigos
validaNumInimigos (Jogo {inimigos = l }) = length l >= 2 

{-|

Recebe um jogo e verifica se todos os inimigos do tipo Fantasma têm 1 vida

=Exemplos
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 5}),Personagem {tipo = Fantasma, vida = 1}]}) = False
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 1}),Personagem {tipo = Fantasma, vida = 1}]}) = True
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = MacacoMalvado, vida = 5}),Personagem {tipo = Fantasma, vida = 1}]}) = True
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 1}),Personagem {tipo = Fantasma, vida = 2}]}) = False
-}
validaVidaFantasma :: Jogo -> Bool
validaVidaFantasma (Jogo {inimigos = []}) = True
validaVidaFantasma (Jogo {inimigos = ((Personagem {tipo = y, vida = x}): t )}) |y == Fantasma = if x == 1 
                                                                                       then validaVidaFantasma (Jogo {inimigos = t })
                                                                                       else False
                                                                               |otherwise = validaVidaFantasma (Jogo {inimigos = t })


{-|

Recebe um jogo e verifica se as escadas são validas
(uma escada não pode começar/terminar em alçapões e pelo menos
uma das suas extremidades tem que ser do tipo Plataforma)

=Exemplos
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Escada    ,Vazio     ,Escada],
                                                          [Escada    ,Escada    ,Escada    ,Plataforma],
                                                          [Vazio     ,Escada    ,Plataforma,Vazio],
                                                          [Alcapao   ,Plataforma,Vazio     ,Plataforma]]}) 
                                                          = True
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Alcapao   ,Vazio     ,Escada],
                                                          [Escada    ,Escada    ,Escada    ,Plataforma],
                                                          [Vazio     ,Escada    ,Plataforma,Vazio],
                                                          [Alcapao   ,Plataforma,Vazio     ,Plataforma]]})0 
                                                          = False
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Escada,Vazio     ,Escada],
                                                          [Escada    ,Escada,Escada    ,Plataforma],
                                                          [Vazio     ,Escada,Plataforma,Vazio],
                                                          [Alcapao   ,Escada,Vazio     ,Plataforma]]}) 
                                                          = False
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Plataforma,Vazio     ,Escada],
                                                          [Escada    ,Escada    ,Escada    ,Plataforma],
                                                          [Vazio     ,Escada    ,Plataforma,Vazio],
                                                          [Alcapao   ,Alcapao   ,Vazio     ,Plataforma]]}) 
                                                          = False
-}

validaEscadas :: Jogo -> Bool
validaEscadas (Jogo {mapa = Mapa _ _ (h:t)}) = validaEscadasAlcapao (h:t) == True && validaPlataforma (transposta (h:t)) == True 

{-|

Recebe uma matriz (lista de listas de "Bloco") e verifica se escadas não começam nem terminam e alçapões

=Exemplos
>>> validaEscadasAlcapao (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Plataforma,Vazio     ,Escada],
                                                                 [Escada    ,Escada    ,Escada    ,Plataforma],
                                                                 [Vazio     ,Escada    ,Plataforma,Vazio],
                                                                 [Alcapao   ,Vazio     ,Vazio     ,Plataforma]]}) 
                                                                 =True
>>> validaEscadasAlcapao (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Plataforma,Vazio     ,Escada],
                                                                 [Escada    ,Escada    ,Escada    ,Plataforma],
                                                                 [Vazio     ,Escada    ,Plataforma,Vazio],
                                                                 [Alcapao   ,Alcapao   ,Vazio     ,Plataforma]]}) 
                                                                 =False
>>> validaEscadasAlcapao (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Alcapao   ,Vazio     ,Escada],
                                                                 [Escada    ,Escada    ,Escada    ,Plataforma],
                                                                 [Vazio     ,Escada    ,Plataforma,Vazio],
                                                                 [Alcapao   ,Vazio     ,Vazio     ,Plataforma]]}) 
                                                                 =False                                                          
                     


-}
validaEscadasAlcapao :: [[Bloco]] -> Bool
validaEscadasAlcapao [h] = True 
validaEscadasAlcapao (h:t) | validaEscadasAlcapaoAux h (head t) == True = validaEscadasAlcapao t
                           | otherwise = False
{-|

Recebe duas linhas seguidas da matriz e verifica se existem escadas e alçapões seguidos nas colunas

=Exemplos
>>> validaEscadasAlcapaoAux [Escada,Vazio,Alcapao] [Escada,Plataforma,Vazio] = True
>>> validaEscadasAlcapaoAux [Escada,Vazio,Alcapao] [Alcapao,Plataforma,Vazio] = False
>>> validaEscadasAlcapaoAux [Escada,Vazio,Alcapao] [Escada,Plataforma,Escada] = False
-}
validaEscadasAlcapaoAux :: [Bloco] -> [Bloco] -> Bool
validaEscadasAlcapaoAux _ [] = True
validaEscadasAlcapaoAux (h1:t1) (h2:t2) | h1 == Alcapao && h2 == Escada = False
                                        | h2 == Alcapao && h1 == Escada = False
                                        | otherwise = validaEscadasAlcapaoAux t1 t2

{-|

Recebe uma matriz e devolve a sua transposta,
Neste caso específico recebe a matriz que representa o mapa e devolve as linhas como colunas e as colunas como linhas

=Exemplos 
>>> transposta [[Plataforma,Vazio     ,Alcapao],
                [Escada    ,Escada    ,Vazio],
                [Escada    ,Plataforma,Vazio]] = [[Plataforma,Escada,Escada],
                                                  [Vazio     ,Escada,Plataforma],
                                                  [Alcapao   ,Vazio ,Vazio]]
-}

transposta :: [[Bloco]] -> [[Bloco]]
transposta [] = []
transposta ([]:_) = []
transposta l = (map head l) : transposta (map tail l)
{-|

Recebe a transposta da matriz de blocos original e verifica se todas as escadas começam ou acabam numa Plataforma

=Exemplos 
>>> validaPlataforma [[Escada    ,Escada,Plataforma],[Plataforma,Escada,Vazio]] = True
>>> validaPlataforma [[Escada,Escada,Plataforma],[Escada,Escada,Vazio]] = False
-}
validaPlataforma :: [[Bloco]] -> Bool
validaPlataforma [] = True
validaPlataforma (h:t) | validaLinhaPlat h == True = validaPlataforma t
                       | otherwise = False    

{-!

Recebe uma lista de blocos (coluna da matriz original) e verifica se todas as escadas começam ou acabam numa Plataforma

=Exemplos
>>> validaLinhaPlat [Escada,Escada,Plataforma] = True
>>> validaLinhaPlat [Escada,Plataforma,Escada] = True
>>> validaLinhaPlat [Escada,Escada,Escada] = False


-}
validaLinhaPlat :: [Bloco] -> Bool
validaLinhaPlat [] = True
validaLinhaPlat [h] = True
validaLinhaPlat (h1:h2:t) | h1 == Plataforma && h2 == Escada = validaLinhaPlat (removeEscada t)
                          | h1 == Escada && h2 == Plataforma = validaLinhaPlat (h2:t)
                          | length (h1:h2:t) == 2 && h1 == Escada && h2 == Escada = False
                          | h1 == Escada && h2 == Escada = validaLinhaPlat (h2:t)
                          | h1 == Escada && h2 /= Plataforma = False
                          | otherwise = validaLinhaPlat (h2:t)

{-|

Recebe uma lista de blocos e devolve a lista sem os primeiros elesmentos caso estes sejam do tipo "Escada"

=Exemplos
>>>removeEscada [Escada,Escada,Plataforma,Escada,Vazio] = [Plataforma,Escada,Vazio]
>>>removeEscada [Alcapao,Escada,Escada,Escada] = [Alcapao,Escada,Escada,Escada]

-}
removeEscada :: [Bloco] -> [Bloco]
removeEscada [] = []
removeEscada (h:t) | h == Escada = removeEscada t
                   | otherwise = (h:t)


{-|

Recebe um jogo e verifica se o tamanho de alçapáo é igual ou superior ao do personagem

=Exemplos
>>> validaLarguraAlcapao (Jogo {jogador = Personagem {tamanho = (1.1,2)}}) = False
>>> validaLarguraAlcapao (Jogo {jogador = Personagem {tamanho = (1,2)}}) = True
>>> validaLarguraAlcapao (Jogo {jogador = Personagem {tamanho = (0.9,2)}}) = True
-}

validaLarguraAlcapao :: Jogo -> Bool
validaLarguraAlcapao (Jogo {jogador = Personagem {tamanho = (x,y) }}) | x <= 1 = True
                                                                      | otherwise = False

{-|

Recebe um jogo e verifica se existem personagens ou colecionáveis "dentro" de plataformas ou alçapões

=Exemplos
>>>validaPosicaoMapa (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]]
                           ,inimigos = [Personagem {posicao = (0,0)}, Personagem {posicao = (2,0)}]
                           ,colecionaveis = [(Martelo,(1,0)),(Moeda,(0,2))]}) 
                            = True
>>>validaPosicaoMapa (Jogo {mapa = Mapa ((1,1),Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]]
                            ,inimigos = [Personagem {posicao = (0,0)}, Personagem {posicao = (2,0)}]
                            ,colecionaveis = [(Martelo,(1,0)),(Moeda,(0,2))]}) 
                            = False
>>>validaPosicaoMapa (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]] 
                            ,inimigos = [Personagem {posicao = (1,1)},Personagem {posicao = (2,0)}]
                            ,colecionaveis = [(Martelo,(1,0)),(Moeda,(0,2))]}) 
                            = False
>>>validaPosicaoMapa (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]]
                            ,inimigos = [Personagem {posicao = (0,0)}, Personagem {posicao = (2,0)}] 
                            ,colecionaveis = [(Martelo,(1,1)),(Moeda,(0,2))]}) 
                            = False
-}

validaPosicaoMapa :: Jogo -> Bool
validaPosicaoMapa jogo' = validaPosicaoMapaJogador jogo' &&
                          validaPosicaoMapaInimigos jogo' &&
                          validaPosicaoMapaColecionaveis jogo'

{-|
Recebe um Jogo e verifica se a posição do jogador é válida

=Exemplos
>>> validaPosicaoMapaJogador (Jogo {mapa = Mapa ((1,2), Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]]}) = True
>>> validaPosicaoMapaJogador (Jogo {mapa = Mapa ((0,1), Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]]}) = False   
>>> validaPosicaoMapaJogador (Jogo {mapa = Mapa ((2,1), Norte) (1,2) [[Vazio,Vazio,Vazio],[Plataforma,Alcapao,Escada],[Vazio,Vazio,Plataforma]]}) = True
-}

validaPosicaoMapaJogador :: Jogo -> Bool
validaPosicaoMapaJogador (Jogo {mapa = Mapa ((x,y),_) _ matriz}) = procuraBloco matriz (x,y) == Vazio || procuraBloco matriz (x,y) == Escada

{-
Recebe um Jogo e verifica se a posição dos inimigos é válida

=Exemplos
>>>validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],[Plataforma,Alcapao,Vazio],[Vazio,Vazio,Vazio]] 
                                   ,inimigos = [Personagem {posicao = (2,0)},Personagem {posicao = (0,0)}]}) 
                                   = True
>>>validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],[Plataforma,Alcapao,Vazio],[Vazio,Vazio,Vazio]]
                                   ,inimigos = [Personagem {posicao = (0,1)},Personagem {posicao = (2,0)}]}) 
                                    = False
>>>validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],[Plataforma,Alcapao,Vazio],[Vazio,Vazio,Vazio]]
                                    ,inimigos = [Personagem {posicao = (2,0)},Personagem {posicao = (1,1)}]}) 
                                    = False 
-}

validaPosicaoMapaInimigos :: Jogo -> Bool
validaPosicaoMapaInimigos (Jogo {mapa = Mapa _ _ matriz,inimigos = []}) = True
validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz
                                ,inimigos = ((Personagem {posicao = (xInimigo, yInimigo)}): tInimigo)}) 
      | procuraBloco matriz (xInimigo,yInimigo) == Vazio  
           || procuraBloco matriz (xInimigo,yInimigo) == Escada
                = validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz
                                                  ,inimigos = tInimigo})
      | otherwise = False

{-|

Recebe um Jogo e verifica se a posição dos colecionáveis é válida

=Exemplos
>>>validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],[Plataforma,Alcapao,Vazio],[Vazio,Vazio,Vazio]]
                                        ,colecionaveis = [(Martelo,(1,0)), (Moeda,(2,0))]}) 
                                        = True
>>>validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],[Plataforma,Alcapao,Vazio],[Vazio,Vazio,Vazio]]
                                         ,colecionaveis = [(Martelo,(1,1)), (Moeda,(0,0))]}) 
                                         = False
>>>validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],[Plataforma,Alcapao,Vazio],[Vazio,Vazio,Vazio]]
                                         ,colecionaveis = [(Martelo,(2,0)), (Moeda,(0,1))]}) 
                                        = False
-}

validaPosicaoMapaColecionaveis :: Jogo -> Bool
validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa _ _ matriz,colecionaveis = []}) = True
validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz
                                      ,colecionaveis = ((col,(xColecionavel,yColecionavel)):tColecionavel)}) 
      | procuraBloco matriz (xColecionavel,yColecionavel) == Vazio || procuraBloco matriz (xColecionavel,yColecionavel) == Escada
        = validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz
                                                ,colecionaveis = (tColecionavel)})
      | otherwise = False

{-| Indica o tipo de Bloco situado numa dada posição

= Exemplos

>>> procuraBloco [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]] (1,1) = Escada
>>> procuraBloco [[Vazio,Plataforma,Vazio],[Vazio,Escada,Vazio],[Plataforma,Plataforma,Plataforma]] (0,0) = Vazio
-}

procuraBloco :: [[Bloco]] -> Posicao -> Bloco
procuraBloco [] _ = Vazio
procuraBloco ((h:t):ls) (x,y) | y > 1 = procuraBloco ls (x,y-1)
                              | x > 1 = procuraBloco [t] (x-1,y)
                              | otherwise = h