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

validaChao :: Jogo -> Bool -- verifica se o mapa tem chão
validaChao (Jogo {mapa = Mapa _ _ (h:t)}) = validaChaoAux (last t)

validaChaoAux :: [Bloco] -> Bool
validaChaoAux [] = True
validaChaoAux (h:t) | h == Plataforma = validaChaoAux t
                 | otherwise = False

{-|
Verifica se todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.

=Exemplos
>>> validaRessalta (Jogo {inimigos = [(Personagem {ressalta = True}), Personagem {ressalta = False}], jogador = Personagem {ressalta = False}}) = False
>>> validaRessalta (Jogo {inimigos = [(Personagem {ressalta = False}), Personagem {ressalta = True}], jogador = Personagem {ressalta = False}}) = False
>>> validaRessalta (Jogo {inimigos = [(Personagem {ressalta = True}), Personagem {ressalta = True}], jogador = Personagem {ressalta = True}}) = False
>>> validaRessalta (Jogo {inimigos = [(Personagem {ressalta = True}), Personagem {ressalta = True}], jogador = Personagem {ressalta = False}}) = True
-}

validaRessalta :: Jogo -> Bool
validaRessalta (Jogo {inimigos = [], jogador = (Personagem {ressalta = y})}) = True
validaRessalta (Jogo {inimigos = ((Personagem {ressalta = x}): t ), jogador = (Personagem {ressalta = y})}) 
      | x == True && y == False = validaRessalta (Jogo {inimigos = t, jogador = (Personagem {ressalta = y})}) 
      | otherwise = False

{-|
Recebe um jogo e verifica se a posição do jogador colide com a posição de algum outro personagem

=Exemplos
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (1,2)}), Personagem {posicao = (1,2)}], jogador = Personagem {posicao = (1,3)}}) = True
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (1,3)}), Personagem {posicao = (1,2)}], jogador = Personagem {posicao = (1,3)}}) = False
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (1,2)}), Personagem {posicao = (1,3)}], jogador = Personagem {posicao = (1,3)}}) = False
>>> validaPosicaoColisao (Jogo {inimigos = [(Personagem {posicao = (2,4)}), Personagem {posicao = (5,1)}], jogador = Personagem {posicao = (3,2)}}) = True
-}

validaPosicaoColisao :: Jogo -> Bool
validaPosicaoColisao (Jogo {inimigos = [], jogador = Personagem {posicao = (x2,y2)}}) = True
validaPosicaoColisao (Jogo {inimigos = ((Personagem {posicao = (x1,y1)}): t ), jogador = Personagem {posicao = (x2,y2)}}) 
      | x1 == x2 && y1 == y2 = False
      | otherwise = validaPosicaoColisao (Jogo {inimigos = t , jogador = Personagem {posicao = (x2,y2)}}) 

{-|
Recebe um jogo e verifica se o jogo tem pelo menos 2 inimigos

=Exemplos
>>> validaNumInimigos (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 5}), Personagem {posicao = (1,2), vida = 1}, Personagem {ressalta = True}]}) = True
>>> validaNumInimigos (Jogo {inimigos = [(Personagem {posicao = (1,2), vida = 1}, Personagem {ressalta = True}]}) = True
>>> validaNumInimigos (Jogo {inimigos = [(Personagem {ressalta = True}]}) = False
>>> validaNumInimigos (Jogo {inimigos = []}) = False
-}

validaNumInimigos :: Jogo -> Bool -- ainda não testada // verifica se o jogo tem pelo menos 2 inimigos
validaNumInimigos (Jogo {inimigos = l }) = length l >= 2 

{-|

Recebe um jogo e verifica se todos os inimigos do tipo Fantasma têm 1 vida

=Exemplos
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 5}), Personagem {tipo = Fantasma, vida = 1}]}) = False
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 1}), Personagem {tipo = Fantasma, vida = 1}]}) = True
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = MacacoMalvado, vida = 5}), Personagem {tipo = Fantasma, vida = 1}]}) = True
>>> validaVidaFantasma (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 1}), Personagem {tipo = Fantasma, vida = 2}]}) = False
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
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Escada,Vazio,Escada],
                                                    [Escada,Escada,Escada,Plataforma],
                                                    [Vazio,Escada,Plataforma,Vazio],
                                                    [Alcapao,Plataforma,Vazio,Plataforma]]}) = True
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Alcapao,Vazio,Escada],
                                                    [Escada,Escada,Escada,Plataforma],
                                                    [Vazio,Escada,Plataforma,Vazio],
                                                    [Alcapao,Plataforma,Vazio,Plataforma]]}) = False
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Escada,Vazio,Escada],
                                                    [Escada,Escada,Escada,Plataforma],
                                                    [Vazio,Escada,Plataforma,Vazio],
                                                    [Alcapao,Escada,Vazio,Plataforma]]}) = False
>>> validaEscadas (Jogo {mapa = Mapa ((1,2),Norte) (1,2) [[Plataforma,Plataforma,Vazio,Escada],
                                                    [Escada,Escada,Escada,Plataforma],
                                                    [Vazio,Escada,Plataforma,Vazio],
                                                    [Alcapao,Alcapao,Vazio,Plataforma]]}) = False
-}

validaEscadas :: Jogo -> Bool
validaEscadas (Jogo {mapa = Mapa _ _ (h:t)}) = validaEscadasAlcapao (h:t) == True && validaPlataforma (transposta (h:t)) == True 


validaEscadasAlcapao :: [[Bloco]] -> Bool
validaEscadasAlcapao [h] = True 
validaEscadasAlcapao (h:t) | validaEscadasAlcapaoAux h (head t) == True = validaEscadasAlcapao t
                    | otherwise = False

validaEscadasAlcapaoAux :: [Bloco] -> [Bloco] -> Bool
validaEscadasAlcapaoAux _ [] = True
validaEscadasAlcapaoAux (h1:t1) (h2:t2) | h1 == Alcapao && h2 == Escada = False
                                 | h2 == Alcapao && h1 == Escada = False
                                 | otherwise = validaEscadasAlcapaoAux t1 t2



transposta :: [[Bloco]] -> [[Bloco]]
transposta [] = []
transposta ([]:_) = []
transposta l = (map head l) : transposta (map tail l)

validaPlataforma :: [[Bloco]] -> Bool
validaPlataforma [] = True
validaPlataforma (h:t) | validaLinhaPlat h == True = validaPlataforma t
                       | otherwise = False    

validaLinhaPlat :: [Bloco] -> Bool
validaLinhaPlat [] = True
validaLinhaPlat [h] = True
validaLinhaPlat (h1:h2:t) | h1 == Plataforma && h2 == Escada = validaLinhaPlat (removeEscada t)
                          | h1 == Escada && h2 == Plataforma = validaLinhaPlat (h2:t)
                          | length (h1:h2:t) == 2 && h1 == Escada && h2 == Escada = False
                          | h1 == Escada && h2 == Escada = validaLinhaPlat (h2:t)
                          | h1 == Escada && h2 /= Plataforma = False
                          | otherwise = validaLinhaPlat (h2:t)

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






-}

validaPosicaoMapa :: Jogo -> Bool
validaPosicaoMapa jogo' = validaPosicaoMapaJogador jogo' &&
                          validaPosicaoMapaInimigos jogo' &&
                          validaPosicaoMapaColecionaveis jogo'

{-|



>>> validaPosicaoMapaJogador (Jogo {mapa = Mapa ((1,2), Norte) (1,2) [[Vazio,Vazio,Vazio], 
                                                                      [Plataforma,Alcapao,Escada],
                                                                      [Vazio,Vazio,Plataforma]]}) = True
>>> validaPosicaoMapaJogador (Jogo {mapa = Mapa ((0,1), Norte) (1,2) [[Vazio,Vazio,Vazio], 
                                                                      [Plataforma,Alcapao,Escada],
                                                                      [Vazio,Vazio,Plataforma]]}) = False   
>>> validaPosicaoMapaJogador (Jogo {mapa = Mapa ((2,0), Norte) (1,2) [[Vazio,Vazio,Vazio], 
                                                                      [Plataforma,Alcapao,Escada],
                                                                      [Vazio,Vazio,Plataforma]]}) = True


-}

validaPosicaoMapaJogador :: Jogo -> Bool
validaPosicaoMapaJogador (Jogo {mapa = Mapa ((x,y),_) _ matriz}) = procuraBlocoInf matriz (x,y) == Vazio

{-

Recebe um Jogo e verifica se a posição dos inimigos é válida

=Exemplos
>>>validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],
                                                          [Plataforma,Alcapao,Vazio],
                                                          [Vazio,Vazio,Vazio]], 
                          inimigos = [Personagem {posicao = (1,0)}, Personagem {posicao = (1,0)}]}) = True
>>>validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],
                                                          [Plataforma,Alcapao,Vazio],
                                                          [Vazio,Vazio,Vazio]], 
                          inimigos = [Personagem {posicao = (1,0)}, Personagem {posicao = (2,0)}]}) = False
>>>validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],
                                                          [Plataforma,Alcapao,Vazio],
                                                          [Vazio,Vazio,Vazio]], 
                          inimigos = [Personagem {posicao = (2,0)}, Personagem {posicao = (1,0)}]}) = False 
-}

validaPosicaoMapaInimigos :: Jogo -> Bool
validaPosicaoMapaInimigos (Jogo {mapa = Mapa _ _ matriz,inimigos = []}) = True
validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz,inimigos = ((Personagem {posicao = (xInimigo, yInimigo)}): tInimigo)}) 
      | procuraBlocoInf matriz (xInimigo,yInimigo) == Vazio 
        = validaPosicaoMapaInimigos (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz
                                ,inimigos = tInimigo})
      | otherwise = False

{-|

Recebe um Jogo e verifica se a posição dos colecionáveis é válida

=Exemplos
>>>validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],
                                                                [Plataforma,Alcapao,Vazio],
                                                                [Vazio,Vazio,Vazio]], 
                               colecionaveis = [(Martelo,(1,0)), (Moeda,(0,0))]}) = True
>>>validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],
                                                                [Plataforma,Alcapao,Vazio],
                                                                [Vazio,Vazio,Vazio]], 
                               colecionaveis = [(Martelo,(2,0)), (Moeda,(0,0))]}) = False
>>>validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((2,0),Norte) (1,2) [[Vazio,Vazio,Escada],
                                                                [Plataforma,Alcapao,Vazio],
                                                                [Vazio,Vazio,Vazio]], 
                               colecionaveis = [(Martelo,(1,0)), (Moeda,(2,0))]}) = False
-}


validaPosicaoMapaColecionaveis :: Jogo -> Bool
validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa _ _ matriz,colecionaveis = []}) = True
validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz,colecionaveis = ((col,(xColecionavel,yColecionavel)):tColecionavel)}) 
      | procuraBlocoInf matriz (xColecionavel,yColecionavel) == Vazio
        = validaPosicaoMapaColecionaveis (Jogo {mapa = Mapa ((xJogador,yJogador), dir) (a,b) matriz
                                     ,colecionaveis = (tColecionavel)})
      | otherwise = False