{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

valida :: Jogo -> Bool
valida = undefined

valida1 :: Jogo -> Bool -- verifica se o mapa tem chão
valida1 (Jogo {mapa = Mapa _ _ (h:t)}) = valida1Aux (last t)

valida1Aux :: [Bloco] -> Bool
valida1Aux [] = True
valida1Aux (h:t) | h == Plataforma = valida1Aux t
                 | otherwise = False

{-|
Verifica se todos os inimigos têm a propriedade ressalta a True, enquanto que o jogador a tem a False.

=Exemplos
>>> valida2 (Jogo {inimigos = [(Personagem {ressalta = True}), Personagem {ressalta = False}], jogador = Personagem {ressalta = False}}) = False
>>> valida2 (Jogo {inimigos = [(Personagem {ressalta = False}), Personagem {ressalta = True}], jogador = Personagem {ressalta = False}}) = False
>>> valida2 (Jogo {inimigos = [(Personagem {ressalta = True}), Personagem {ressalta = True}], jogador = Personagem {ressalta = True}}) = False
>>> valida2 (Jogo {inimigos = [(Personagem {ressalta = True}), Personagem {ressalta = True}], jogador = Personagem {ressalta = False}}) = True
-}

valida2 :: Jogo -> Bool
valida2 (Jogo {inimigos = [], jogador = (Personagem {ressalta = y})}) = True
valida2 (Jogo {inimigos = ((Personagem {ressalta = x}): t ), jogador = (Personagem {ressalta = y})}) 
      | x == True && y == False = valida2 (Jogo {inimigos = t, jogador = (Personagem {ressalta = y})}) 
      | otherwise = False

{-|
Recebe um jogo e verifica se a posição do jogador colide com a posição de algum outro personagem

=Exemplos
>>> valida3 (Jogo {inimigos = [(Personagem {posicao = (1,2)}), Personagem {posicao = (1,2)}], jogador = Personagem {posicao = (1,3)}}) = True
>>> valida3 (Jogo {inimigos = [(Personagem {posicao = (1,3)}), Personagem {posicao = (1,2)}], jogador = Personagem {posicao = (1,3)}}) = False
>>> valida3 (Jogo {inimigos = [(Personagem {posicao = (1,2)}), Personagem {posicao = (1,3)}], jogador = Personagem {posicao = (1,3)}}) = False
>>> valida3 (Jogo {inimigos = [(Personagem {posicao = (2,4)}), Personagem {posicao = (5,1)}], jogador = Personagem {posicao = (3,2)}}) = True
-}

valida3 :: Jogo -> Bool
valida3 (Jogo {inimigos = [], jogador = Personagem {posicao = (x2,y2)}}) = True
valida3 (Jogo {inimigos = ((Personagem {posicao = (x1,y1)}): t ), jogador = Personagem {posicao = (x2,y2)}}) 
      | x1 == x2 && y1 == y2 = False
      | otherwise = valida3 (Jogo {inimigos = t , jogador = Personagem {posicao = (x2,y2)}}) 

{-|
Recebe um jogo e verifica se o jogo tem pelo menos 2 inimigos

=Exemplos
>>> valida4 (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 5}), Personagem {posicao = (1,2), vida = 1}, Personagem {ressalta = True}]}) = True
>>> valida4 (Jogo {inimigos = [(Personagem {posicao = (1,2), vida = 1}, Personagem {ressalta = True}]}) = True
>>> valida4 (Jogo {inimigos = [(Personagem {ressalta = True}]}) = False
>>> valida4 (Jogo {inimigos = []}) = False
-}

valida4 :: Jogo -> Bool -- ainda não testada // verifica se o jogo tem pelo menos 2 inimigos
valida4 (Jogo {inimigos = l }) = length l >= 2 

{-|

Recebe um jogo e verifica se todos os inimigos do tipo Fantasma têm 1 vida

=Exemplos
>>> valida5 (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 5}), Personagem {tipo = Fantasma, vida = 1}]}) = False
>>> valida5 (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 1}), Personagem {tipo = Fantasma, vida = 1}]}) = True
>>> valida5 (Jogo {inimigos = [(Personagem {tipo = MacacoMalvado, vida = 5}), Personagem {tipo = Fantasma, vida = 1}]}) = True
>>> valida5 (Jogo {inimigos = [(Personagem {tipo = Fantasma, vida = 1}), Personagem {tipo = Fantasma, vida = 2}]}) = False
-}
valida5 :: Jogo -> Bool
valida5 (Jogo {inimigos = []}) = True
valida5 (Jogo {inimigos = ((Personagem {tipo = y, vida = x}): t )}) |y == Fantasma = if x == 1 
                                                                                       then valida5 (Jogo {inimigos = t })
                                                                                       else False
                                                                    |otherwise = valida5 (Jogo {inimigos = t })

valida6 :: Jogo -> Bool
valida6 (Jogo {mapa = Mapa _ _ (h:t)}) = validaAlcapao (h:t) == True && validaPlataforma (transposta (h:t)) == True 


validaAlcapao :: [[Bloco]] -> Bool
validaAlcapao [h] = True 
validaAlcapao (h:t) | validaAlcapaoAux h (head t) == True = validaAlcapao t
                    | otherwise = False

validaAlcapaoAux :: [Bloco] -> [Bloco] -> Bool
validaAlcapaoAux _ [] = True
validaAlcapaoAux (h1:t1) (h2:t2) | h1 == Alcapao && h2 == Escada = False
                                 | h2 == Alcapao && h1 == Escada = False
                                 | otherwise = validaAlcapaoAux t1 t2



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
