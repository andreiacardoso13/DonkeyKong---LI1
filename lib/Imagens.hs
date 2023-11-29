module Imagens where
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
               Just escada              <- loadJuicyJPG "imagens/ladder.png"
               Just alcapao             <- loadJuicyJPG "imagens/alcapao.png"
               Just plataforma          <- loadJuicyJPG "imagens/plataforma.png"
               Just estrela             <- loadJuicyJPG "imagens/star.png"
               Just moeda               <- loadJuicyJPG "imagens/coin.png"
               Just vazio               <- loadJuicyJPG "imagens/vazio.png"
               let images = [escada, alcapao, plataforma, estrela, moeda, vazio]
               return images