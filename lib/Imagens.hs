module Imagens where
import Graphics.Gloss
import Graphics.Gloss.Juicy

data Imagens = Imagens {
    escada :: Picture,
    alcapao :: Picture,
    plataforma :: Picture,
    estrela :: Picture,
    moeda :: Picture,
    vazio :: Picture
    }

getImages :: IO [Picture]
getImages = do
               Just escada              <- loadJuicyPNG "imagens/ladder.png"
               Just alcapao             <- loadJuicyPNG "imagens/alcapao.png"
               Just plataforma          <- loadJuicyPNG "imagens/plataforma.png"
               Just estrela             <- loadJuicyPNG "imagens/estrela.png"
               Just moeda               <- loadJuicyPNG "imagens/moeda.png"
               Just vazio               <- loadJuicyPNG "imagens/vazio.png"
               let images = [escada, alcapao, plataforma, estrela, moeda, vazio]
               return images