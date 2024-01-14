{-|
Module      : Tarefa2_202324li1g005_Spec
Description : Testes da Tarefa 2
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização dos testes da Tarefa 2 de LI1 em 2023/24.
-}

module Tarefa2_202324li1g005_Spec where

import Tarefa2
import LI12324
import Test.HUnit

-- | Exemplo de mapa válido
mapa01 :: Mapa
mapa01 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Plataforma, Plataforma, Plataforma, Alcapao   , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

-- | Exemplo de mapa inválido (escadas têm extremidades em alçapões)
mapa02 :: Mapa
mapa02 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Alcapao   , Plataforma, Plataforma, Plataforma, Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Plataforma, Plataforma, Plataforma, Alcapao   , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]


-- | Exemplo de mapa inválido (sem chão)
mapa03 :: Mapa
mapa03 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Plataforma, Plataforma, Plataforma, Alcapao   , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Plataforma, Plataforma, Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

-- | Exemplo de mapa inválido (Escadas não têm pelo menos uma extremidade em plataformas)
mapa04 :: Mapa
mapa04 =
  Mapa
    ((8.5, 6.5), Este)
    (5, 1.5)
    [ [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     , Vazio     ],
      [Vazio     , Plataforma, Plataforma, Plataforma, Alcapao   , Plataforma, Plataforma, Plataforma, Plataforma, Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Vazio     , Escada    , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ],
      [Plataforma, Plataforma, Vazio     , Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
    ]

inimigoModelo =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Fantasma,
      posicao = (2.5, 7.6),
      direcao = Este,
      tamanho = (1, 1),
      emEscada = False,
      ressalta = True,
      vida = 1,
      pontos = 0,
      aplicaDano = (False, 0)
    }

jogadorParado =
  Personagem
    { velocidade = (0.0, 0.0),
      tipo = Jogador,
      posicao = (8.5, 6.5),
      direcao = Oeste,
      tamanho = (0.8, 0.8),
      emEscada = False,
      ressalta = False,
      vida = 10,
      pontos = 0,
      aplicaDano = (False, 0)
    }


jogo01 :: Jogo
jogo01 =
  Jogo
    { mapa = mapa01,
      inimigos = [inimigoModelo, inimigoModelo],
      colecionaveis = [],
      jogador = jogadorParado
    }


teste1 = "T1: Jogo que respeita todas as regras é válido" ~: True ~=? valida jogo01

teste2 = "T2: Jogo que não tem chão é inválido" ~: False ~=? valida jogo01{mapa=mapa03} --ou validaChao 

teste3 = "T3: Jogo onde inimigos têm ressalta False é inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo{ressalta=False}, inimigoModelo]}

teste4 = "T4: Jogo onde jogador tem ressalta True é inválido" ~: False ~=? valida jogo01 {jogador = jogadorParado{ressalta = True}}

teste5 = "T5: Se a posicao inicial do jogador colide com a posição inicial de um inimigo o jogo é inválido" ~: False ~=? valida jogo01 {jogador = jogadorParado {posicao = (2.7,7.4)}}

teste6 = "T6: Se o número de inimigos for menor a dois o jogo é inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo]}

teste7 = "T7: Se a vida inicial dos inimigos for diferente de 1 o jogo é inválido" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {vida = 2}, inimigoModelo]}

teste8 = "T8: Escadas não podem ter extremidades em alçapões" ~: False ~=? valida jogo01{mapa = mapa02}

teste9 = "T9: Escadas têm de ter pelo menos uma das extremidades do tipo Plataforma" ~: False ~=? valida jogo01{mapa = mapa04}

teste10 = "T10: Se a largura do jogador for maior do que o alçapão o jogo é inválido" ~: False ~=? valida jogo01{jogador = jogadorParado {tamanho = (1.5,1)}}

teste11 = TestLabel "T11" $ test [teste11A,teste11B,teste11C,teste11D,teste11E,teste11F]
  where 
    teste11A = "T11A: Jogadores não podem estar 'dentro' de plataformas" ~: False ~=? valida jogo01 {jogador = jogadorParado {posicao = (0.5,11.5)}}
    teste11B = "T11B: Jogadores não podem estar 'dentro' de alçapões" ~: False ~=? valida jogo01 {jogador = jogadorParado {posicao = (4.5,8.5)}}
    teste11C = "T11C: Inimigos não podem estar 'dentro' de plataformas" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {posicao = (0.5,11.5)}, inimigoModelo]}
    teste11D = "T11D: Inimigos não podem estar 'dentro' de alçapões" ~: False ~=? valida jogo01 {inimigos = [inimigoModelo {posicao = (4.5,8.5)}, inimigoModelo]}
    teste11E = "T11E: Colecionaveis não podem estar 'dentro' de plataformas" ~: False ~=? valida jogo01 {colecionaveis = [(Moeda,(0.5,11.5))]}
    teste11F = "T11F: Colecionaveis não podem estar 'dentro' de alçapões" ~: False ~=? valida jogo01 {colecionaveis = [(Moeda,(4.5,8.5))]}

testesTarefa2 = test [teste1,teste2,teste3,teste4,teste5,teste6,teste7,teste8,teste9,teste10,teste11]
