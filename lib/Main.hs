module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Imagens
import Mapa


{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

data Estado = Estado {jogo :: Jogo, imagens :: Imagens}


main :: IO ()
main = do
  images <- getImages
  play janela
       bg
       fr
       (estadoInicial images)
       desenhaEstado
       reageEvento
       reageTempo

--Um da matriz equivale a 53 do Gloss

janela :: Display
janela = InWindow
  "Mapa"     -- título de janela    
  (1484,954)  -- dimensão da janela // 53*53 cada bloco
  (255,60)    -- posição no ecrã

bg :: Color
bg = black

fr :: Int
fr = 20



estadoInicial :: Imagens -> Estado
estadoInicial images = Estado {jogo = j1, imagens = images}

desenhaEstado :: Estado -> Picture
desenhaEstado s = Pictures((desenhaMapa1 (-715.5,450.5) s)++desenhaJogador s)

desenhaMapa1 :: (Float,Float) -> Estado -> [Picture]
desenhaMapa1 _ (Estado {jogo = (Jogo {mapa = Mapa a b []})}) = []
desenhaMapa1 (x,y) (Estado {jogo = (Jogo {mapa = Mapa a b (h:t)}), imagens = imgs}) = (desenhaLinhas1 (x,y) imgs h) ++ (desenhaMapa1 (x,y-53) (Estado {jogo = (Jogo {mapa = Mapa a b t}), imagens = imgs}))

desenhaLinhas1 :: (Float,Float) -> Imagens -> [Bloco] -> [Picture] 
desenhaLinhas1 _ _ [] = []
desenhaLinhas1 (x,y) imgs (h : t) | h == Escada = (Translate x y (getImagem Ladder imgs)) : desenhaLinhas1 (x+53,y) imgs t 
                                  | h == Plataforma = (Translate x y(getImagem Platform imgs)): desenhaLinhas1 (x+53,y) imgs t 
                                  | h == Alcapao = (Translate x y(getImagem Trapdoor imgs)): desenhaLinhas1 (x+53,y) imgs t 
                                  | h == Vazio = desenhaLinhas1 (x+53,y) imgs t 

desenhaJogador :: Estado -> [Picture]
desenhaJogador est | (emEscada (jogador(jogo est))) == True = [Translate (-742 + x) (-477 + y) (getImagem MarioClimbing2 (imagens est))]
                   | fst (aplicaDano (jogador(jogo est))) == True && (direcao (jogador (jogo (est)))) == Este = [Translate (-742 + x) (-477 + y) (getImagem MarioHammerRight1 (imagens est))]
                   | fst (aplicaDano (jogador(jogo est))) == True && (direcao (jogador (jogo (est)))) == Oeste = [Translate (-742 + x) (-477 + y) (getImagem MarioHammerLeft1 (imagens est))]  
                   | (direcao (jogador (jogo (est)))) == Este && (velocidade (jogador (jogo (est)))) /= (0,0) = [Translate (-742 + x) (-477 + y) (getImagem MarioWalkingRight1 (imagens est))]
                   | (direcao (jogador (jogo (est)))) == Oeste && (velocidade (jogador (jogo (est)))) /= (0,0) = [Translate (-742 + x) (-477 + y) (getImagem MarioWalkingLeft1 (imagens est))]
                   | (direcao (jogador (jogo (est)))) == Este  = [Translate (-742 + x) (-477 + y) (getImagem MarioStandingRight (imagens est))]
                   | (direcao (jogador (jogo (est)))) == Oeste  = [Translate (-742 + x) (-477 + y) (getImagem MarioStandingLeft(imagens est))]
                   | otherwise = [Translate (-742 + x) (-477 + y) (getImagem MarioHammerLeft2 (imagens est))]
    where x = realToFrac $ (fst (posicao (jogador(jogo est)))) * 53
          y = realToFrac $ (snd (posicao (jogador(jogo est)))) * 53

{-

converteCoordenadasPers :: Picture -> Picture
converteCoordenadasPers img = Translate (x - 742) (y - 477) 

converteCoordenadasMapa :: Posicao -> Posicao 
converteCoordenadasMapa (x,y) = (x - 715.5, y - 450.5)

-}


reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

reageTempo :: Float -> Estado -> Estado
reageTempo _ s = s


