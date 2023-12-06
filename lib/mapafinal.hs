module Mapa where
import LI12324
import Main
import Graphics.Gloss

p::Bloco
p = Plataforma

v::Bloco
v=Vazio

e::Bloco
e=Escada

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
                 [[p, p, p, p, p, p, p, p, p, p]
                 ,[v, v, v, v, v, v, v, v, v, v]
                 ,[v, v, v, v, v, v, v, v, v, v]
                 ,[p, p, v, v, v, v, p, p, p, p]
                 ,[v, v, v, v, v, v, v, v, e, v]
                 ,[v, v, v, v, v, v, v, v, e, v]
                 ,[p, p, p, p, p, p, p, p, p, p]
                 ]

