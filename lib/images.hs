module Images where

import Graphics.Gloss
import Graphics.Gloss.Juicy

data Bloco = Escada | Plataforma | Alcapao | Vazio
data Personagem = Personagem
data Entidade = MacacoMalvado | Fantasma | Jogador

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
               escada              <- loadJuicyJPG "imagens/ladder.bmp"
               alcapao             <- loadJuicyJPG "imagens/alcapao.bmp"
               plataforma          <- loadJuicyJPG "imagens/plataforma.bmp"
               estrela             <- loadJuicyJPG "imagens/star.bmp"
               moeda               <- loadJuicyJPG "imagens/coin.bmp"
               vazio               <- loadJuicyJPG "imagens/Vazio.bmp"
               let images = [escada, alcapao, plataforma, estrela, moeda]
               return images