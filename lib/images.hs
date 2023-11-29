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
               escada              <- loadJuicyJPG "imagens/ladder.png"
               alcapao             <- loadJuicyJPG "imagens/alcapao.png"
               plataforma          <- loadJuicyJPG "imagens/plataforma.png"
               estrela             <- loadJuicyJPG "imagens/star.png"
               moeda               <- loadJuicyJPG "imagens/coin.png"
               vazio               <- loadJuicyJPG "imagens/vazio.png"
               let images = [escada, alcapao, plataforma, estrela, moeda]
               return images