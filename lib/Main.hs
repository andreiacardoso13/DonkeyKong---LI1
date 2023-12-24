module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Imagens
import Mapa
import Graphics.Gloss.Juicy -- precisa apenas de ficar no Imagens ACHO


{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

data Estado = Estado {jogo :: Jogo}

type EstadoGloss = (Estado,Picture)



main :: IO ()
main = do 
       -- imagens <- getImages
        maybeEscada <- loadJuicyPNG "imagens/ladder.png"
        case maybeEscada of 
          Just escada -> play
              janela                          -- janela onde irá decorrer o jogo
              bg                              -- cor do fundo da janela
              fr                              -- frame rate
              (estadoGlossInicial maybeEscada)                 -- define estado inicial do jogo
              desenhaEstado                   -- desenha o estado do jogo
              reageEvento                     -- reage a um evento
              reageTempo                      -- reage ao passar do tempo
          Nothing -> putStrLn "Erro ao carregar imagem"

janela :: Display
janela = InWindow
  "Mapa"     -- título de janela    
  (500,500)  -- dimensão da janela
  (10,10)    -- posição no ecrã

bg :: Color
bg = white

fr :: Int
fr = 20

estadoInicial :: Estado
estadoInicial = Estado {jogo = j1}

estadoGlossInicial :: Maybe Picture -> EstadoGloss
estadoGlossInicial maybeImagem = case maybeImagem of
  Just imagem -> (estadoInicial,imagem)
  Nothing -> error "Erro ao carregar imagem"

desenhaEstado :: EstadoGloss -> Picture
desenhaEstado (jog,z) = z

reageEvento :: Event -> EstadoGloss -> EstadoGloss
reageEvento _ s = s

reageTempo :: Float -> EstadoGloss -> EstadoGloss
reageTempo n (jog,z) = (jog,z)


