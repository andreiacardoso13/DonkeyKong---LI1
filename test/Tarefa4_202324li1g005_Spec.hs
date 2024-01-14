module Tarefa4_202324li1g005_Spec where

import Tarefa4
import LI12324
import Test.HUnit

-- | Exemplo de jogo para testes
j = Jogo {mapa = mapa1
         ,inimigos = [en1, en2, en3, en4, en5]
         ,colecionaveis = [(Moeda,(4.5,10.5)),(Moeda,(21.5,7.5)),(Martelo, (5.5,7.5)),(Moeda,(24.5,10.5)),(Moeda,(13.5,10.5))]
         ,jogador = jgd 
         }


-- | Exemplo de mapa
mapa1 = Mapa ((14,16.5), Este) (14,1.5) [[Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ],
                                         [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ],
                                         [Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ],
                                         [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ],
                                         [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ],
                                         [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]

-- | Exemplo de inimigo
en1 = Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao=(14,4), direcao = Este, tamanho = (2.4,2), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de inimigo
en2 = Personagem {velocidade=(-1.5,0), tipo= Fantasma, posicao=(5.5,4.5), direcao = Oeste, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de inimigo
en3 = Personagem {velocidade=(1.5,0), tipo= Fantasma, posicao=(14.5,1.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de inimigo
en4 = Personagem {velocidade=(1.5,0), tipo= Fantasma, posicao=(22.5,7.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de inimigo
en5 = Personagem {velocidade=(1.5,0), tipo= Fantasma, posicao=(14.5,7.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}

-- | Exemplo de jogador
jgd = Personagem {velocidade=(5,0), tipo=Jogador, posicao=(14,16.5), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 500, aplicaDano = (False,0)}


teste1 = "T1: Atualiza as direções e velocidades de todas as personagens" ~: j {inimigos = [en1, en2 {velocidade = (5,0), direcao = Este}, en3 {velocidade = (0,0)}, en4 {velocidade = (-5,0), direcao = Oeste}, en5], jogador = jgd {velocidade = (5,-5)} } ~=? atualiza [Nothing, Just AndarDireita, Just Parar, Just AndarEsquerda, Nothing] (Just Saltar) j

teste2 = "T2: Mantém as direções e velocidades dos inimigos" ~: [en1, en2, en3, en4, en5] ~=? atualizaInimigos [en1, en2, en3, en4, en5] [Nothing, Nothing, Nothing, Nothing, Nothing]

teste3 = "T3: Atualiza a direção e velocidade do jogador" ~: jgd {velocidade = (0,-5), direcao = Norte, emEscada = True} ~=? atualizaPersonagem jgd (Just Subir)

testesTarefa4 = test [teste1,teste2,teste3]
