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

