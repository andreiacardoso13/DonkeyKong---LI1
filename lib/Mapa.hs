module Mapa where
import LI12324
import Graphics.Gloss


mapaInicial :: Mapa
mapaInicial = Mapa ((0,0), Este) (0,0) [[Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ],
                                        [Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ,Vazio     ],
                                        [Vazio     ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Alcapao   ,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Vazio     ],
                                        [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ],
                                        [Vazio     ,Escada    ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Vazio     ,Escada    ,Vazio     ],
                                        [Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma,Plataforma]]


mapaPrincipal :: Mapa 
mapaPrincipal = Mapa ((0.5,5.5), Oeste) (0.5,2.5)
                 [[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
                 ,[Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ]
                 ,[Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     ]
                 ,[Plataforma, Plataforma, Vazio     , Vazio     , Vazio     , Vazio     , Plataforma, Plataforma, Plataforma, Plataforma]
                 ,[Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ]
                 ,[Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Vazio     , Escada    , Vazio     ]
                 ,[Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma, Plataforma]
                 ]

m1 :: Mapa
m1 = Mapa ((0,0), Este) (0,0) [[Vazio     , Plataforma, Vazio     ],
                               [Vazio     , Escada    , Vazio     ],
                               [Plataforma, Plataforma, Plataforma]]

m2 :: Mapa
m2 = Mapa ((0,0), Este) (0,0) [[Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                               [Plataforma,Vazio     ,Plataforma,Plataforma],
                               [Vazio     ,Vazio     ,Vazio     ,Vazio     ],
                               [Plataforma,Plataforma,Plataforma,Plataforma]]

--exemplo de jogo

j1 :: Jogo
j1 = Jogo {mapa = m1
          ,inimigos = [Personagem {velocidade=(1,0)
                                  ,tipo=Fantasma
                                  ,posicao=(2.5,1.5)
                                  ,direcao = Este
                                  ,tamanho = (1,1)
                                  ,emEscada=False
                                  ,ressalta=True
                                  ,vida=1
                                  ,pontos=0
                                  ,aplicaDano=(False,0)
                                   }
                      ]
          ,colecionaveis = [(Moeda,(2.5,0.5))]
          ,jogador = Personagem {velocidade=(1,0)
                                ,tipo=Fantasma
                                ,posicao=(0.5,1.5)
                                ,direcao = Este
                                ,tamanho = (1,1)
                                ,emEscada=False
                                ,ressalta= False
                                ,vida=3
                                ,pontos=0
                                ,aplicaDano=(False,0)
                                }
          }
                        

