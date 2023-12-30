module Mapa where
import LI12324
import Graphics.Gloss
import Imagens
import Data.Maybe

-- [("escada",escada), ("alcapao",alcapao), ("plataforma",plataforma), ("estrela",estrela),("moeda",moeda), ("martelo",martelo), ("ghostLeft1",ghostLeft1), ("ghostLeft2",ghostLeft2),("ghostRight1",ghostRight1), ("ghostRight2",ghostRight2), ("marioClimbing1",marioClimbing1),("marioClimbing2",marioClimbing2), ("marioHammerLeft1",marioHammerLeft1),("marioHammerLeft2",marioHammerLeft2), ("marioHammerLeft3",marioHammerLeft3),("marioHammerLeft4",marioHammerLeft4), ("marioHammerRight1",marioHammerRight1),("marioHammerRight2",marioHammerRight2), ("marioHammerRight3",marioHammerRight3),("marioHammerRight4",marioHammerRight4), ("marioJumpingLeft1",marioJumpingLeft1),("marioJumpingRight1",marioJumpingRight1), ("marioStandingBack",marioStandingBack),("marioStandingLeft",marioStandingLeft), ("marioStandingRight",marioStandingRight),("marioWalkingLeft1",marioWalkingLeft1), ("marioWalkingRight1",marioWalkingRight1),("monkeyDefeated",monkeyDefeated), ("monkeyFalling",monkeyFalling)]

getImagem :: Imagem -> Imagens -> Picture
getImagem im imgs = fromJust (lookup im imgs)

desenhaBlocos :: Imagens -> Imagem -> Posicao -> Picture
desenhaBlocos imgs b (x,y) = getImagem b imgs

desenhaLinhas :: Imagens -> [Imagem] -> [Posicao] -> [Picture]
desenhaLinhas imgs (h:t) (p:ps) = desenhaBlocos imgs h p : desenhaLinhas imgs t ps

desenhaMapa :: Imagens -> [[Imagem]] -> [[Posicao]] -> [[Picture]]
desenhaMapa imgs ((h:t):ls) ((p:ps):as) = desenhaLinhas imgs (h:t) (p:ps) : desenhaMapa imgs ls as


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
j1 = Jogo {mapa = mapaInicial
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
                                ,posicao=(0.5,16.5)
                                ,direcao = Este
                                ,tamanho = (1,1)
                                ,emEscada=False
                                ,ressalta= False
                                ,vida=3
                                ,pontos=0
                                ,aplicaDano=(False,0)
                                }
          }
                        

