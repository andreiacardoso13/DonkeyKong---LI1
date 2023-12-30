module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Imagens
import Mapa
import Data.Fixed


{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

data Estado = Estado {jogo :: Jogo, imagens :: Imagens, tempo :: Float} -- o tempo aumenta 4 por segundo, serve para alterar as imagens automaticamente


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
estadoInicial images = Estado {jogo = j1, imagens = images, tempo = 1}

desenhaEstado :: Estado -> Picture
desenhaEstado s = Pictures((desenhaMapa1 (-715.5,450.5) s)++desenhaJogador s++desenhaInimigos s++desenhaColecionaveis s++desenhaEstrela s++desenhaVida s++desenhaPontos s)





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
desenhaJogador est | direcao (jogador (jogo est)) == Este = desenhaJogEste est
                   | direcao (jogador (jogo est)) == Oeste = desenhaJogOeste est
                   | otherwise = desenhaJogNorteSul est
                  
      
desenhaJogNorteSul :: Estado -> [Picture]
desenhaJogNorteSul est | emEscada (jogador (jogo est)) == True = if ePar (tempo est)
                                                                      then [desenhaJogadorAux est MarioClimbing1]
                                                                      else [desenhaJogadorAux est MarioClimbing2]
                          | otherwise = [desenhaJogadorAux est MarioStandingBack]

desenhaJogEste :: Estado -> [Picture]
desenhaJogEste est | snd velocidadeJog /= 0 = [desenhaJogadorAux est MarioJumpingRight1] -- está a saltar/cair para a direita
                   | velocidadeJog /= (0,0) && aplicaDanoJog == True = if ePar(tempo est) 
                                                                         then [Translate 20 0 (desenhaJogadorAux est MarioHammerRight1)] -- está a andar para a direita com o martelo para baixo
                                                                         else [Translate 0 20 (desenhaJogadorAux est MarioHammerRight4)] -- está a andar para a direita com o martelo para cima
                   | velocidadeJog /= (0,0) = if ePar (tempo est ) 
                                                then [desenhaJogadorAux est MarioWalkingRight1] -- está a andar para a direita (desenho a andar)
                                                else [desenhaJogadorAux est MarioStandingRight] -- está a andar para a direita (desenho parado)
                   | velocidadeJog == (0,0) && aplicaDanoJog == True = if ePar(tempo est) 
                                                                         then [Translate 20 0 (desenhaJogadorAux est MarioHammerRight1)] --está parado e com martelo para baixo
                                                                         else [Translate 0 20 (desenhaJogadorAux est MarioHammerRight2)] --está parado e com martelo para cima
                   | otherwise = [desenhaJogadorAux est MarioStandingRight] -- mario parado virado pra direita
    where velocidadeJog = velocidade (jogador (jogo est))
          aplicaDanoJog = fst (aplicaDano(jogador(jogo est)))


desenhaJogOeste :: Estado -> [Picture]
desenhaJogOeste est | snd velocidadeJog /= 0 = [desenhaJogadorAux est MarioJumpingLeft1] -- está a saltar/cair para a esquerda
                    | velocidadeJog /= (0,0) && aplicaDanoJog == True = if ePar(tempo est) 
                                                                         then [Translate (-20) 0 (desenhaJogadorAux est MarioHammerLeft1)] -- está a andar para a esquerda com o martelo para baixo
                                                                         else [Translate 0 20(desenhaJogadorAux est MarioHammerLeft4)] -- está a andar para a esquerda com o martelo para cima
                    | velocidadeJog /= (0,0) = if ePar (tempo est ) 
                                                then [desenhaJogadorAux est MarioWalkingLeft1] -- está a andar para a esquerda (desenho a andar)
                                                else [desenhaJogadorAux est MarioStandingLeft] -- está a andar para a esquerda (desenho parado)
                    | velocidadeJog == (0,0) && aplicaDanoJog == True = if ePar(tempo est) 
                                                                          then [Translate (-20) 0 (desenhaJogadorAux est MarioHammerLeft1)] --está parado virado para a esquerda e com martelo para baixo
                                                                          else [Translate 0 20 (desenhaJogadorAux est MarioHammerLeft2)] --está parado virado para a esquerda e com martelo para cima
                    | otherwise = [desenhaJogadorAux est MarioStandingLeft] -- mario parado virado pra esquerda
    where velocidadeJog = velocidade (jogador (jogo est))
          aplicaDanoJog = fst (aplicaDano(jogador(jogo est)))


desenhaJogadorAux :: Estado -> Imagem -> Picture
desenhaJogadorAux est img = Translate (x - 742) (477 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (posicao (jogador(jogo est)))) * 53
          y = realToFrac $ (snd (posicao (jogador(jogo est)))) * 53








desenhaInimigos :: Estado -> [Picture]
desenhaInimigos (Estado {jogo = Jogo {inimigos = []}, imagens = imgs, tempo = tp}) = []
desenhaInimigos (Estado {jogo = jog, imagens = imgs, tempo = tp}) = desenhaInimigosAux (Estado {jogo = jog {inimigos = take 1 (inimigos jog)}, imagens = imgs, tempo = tp}) : (desenhaInimigos (Estado {jogo = jog {inimigos = drop 1 (inimigos jog)}, imagens = imgs, tempo = tp}))

desenhaInimigosAux :: Estado -> Picture
desenhaInimigosAux est | direcao (head (inimigos (jogo est))) == Este = if ePar (tempo est) 
                                                                          then desenhaInimAux est GhostRight1
                                                                          else desenhaInimAux est GhostRight2
                       | otherwise = if ePar (tempo est) 
                                       then desenhaInimAux est GhostLeft1
                                       else desenhaInimAux est GhostLeft2


desenhaInimAux :: Estado -> Imagem -> Picture
desenhaInimAux est img = Translate (x - 742) (480 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (posicao (head (inimigos(jogo est))))) * 53
          y = realToFrac $ (snd (posicao (head (inimigos(jogo est))))) * 53










desenhaColecionaveis :: Estado -> [Picture]
desenhaColecionaveis (Estado {jogo = Jogo {colecionaveis= []}, imagens = imgs, tempo = tp}) = []
desenhaColecionaveis (Estado {jogo = jog, imagens = imgs, tempo = tp}) = desenhaColecionaveisAux (Estado {jogo = jog {colecionaveis = take 1 (colecionaveis jog)}, imagens = imgs, tempo = tp}) : (desenhaColecionaveis (Estado {jogo = jog {colecionaveis = drop 1 (colecionaveis jog)}, imagens = imgs, tempo = tp}))


desenhaColecionaveisAux :: Estado -> Picture
desenhaColecionaveisAux est | fst (head (colecionaveis (jogo est))) == Moeda = desenhaColecAux est Coin
                            | otherwise = desenhaColecAux est Hammer 

desenhaColecAux :: Estado -> Imagem -> Picture
desenhaColecAux est img = Translate (x - 742) (477 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (snd (head (colecionaveis (jogo est))))) * 53
          y = realToFrac $ (snd (snd (head (colecionaveis (jogo est))))) * 53








--por a estrela mais para cima talvez (diminuir ao y)
desenhaEstrela :: Estado -> [Picture]
desenhaEstrela s = [Translate (x - 742) (477 - y) (getImagem Estrela (imagens s))]
  where x = 14 * 53
        y = 1.5 * 53






desenhaVida :: Estado -> [Picture] 
desenhaVida s | vida (jogador (jogo s)) == 0 = [Translate (-630) 340 (Scale 0.3 0.3 (getImagem ZeroVidas (imagens s)))]
              | vida (jogador (jogo s)) == 1 = [Translate (-630) 340 (Scale 0.3 0.3 (getImagem UmaVida (imagens s)))]
              | vida (jogador (jogo s)) == 2 = [Translate (-630) 340 (Scale 0.3 0.3 (getImagem DuasVidas (imagens s)))]
              | otherwise = [Translate (-630) 340 (Scale 0.3 0.3 ( getImagem TresVidas (imagens s)))]



desenhaPontos :: Estado -> [Picture]
desenhaPontos est = desenhaPontosImg est ++ desenhaPontosNum est

desenhaPontosImg :: Estado -> [Picture]
desenhaPontosImg est = [Translate (-631) (420) (getImagem Score (imagens est))]

desenhaPontosNum :: Estado -> [Picture]
desenhaPontosNum est = desenhaPontosNum1 est ++ desenhaPontosNum2 est ++ desenhaPontosNum3 est++ desenhaPontosNum4 est++ desenhaPontosNum5 est

desenhaPontosNum1 :: Estado -> [Picture]
desenhaPontosNum1 est = verificaNumero (div pt 10000) est
   where pt = (pontos(jogador(jogo est)))

desenhaPontosNum2 :: Estado -> [Picture]
desenhaPontosNum2 est = map (Translate 30 0) (verificaNumero (mod (div pt 1000) 10) est)
   where pt = (pontos(jogador(jogo est)))

desenhaPontosNum3 :: Estado -> [Picture]
desenhaPontosNum3 est = map (Translate 60 0) (verificaNumero (mod (div pt 100) 10) est)
   where pt = (pontos(jogador(jogo est)))

desenhaPontosNum4 :: Estado -> [Picture]
desenhaPontosNum4 est = map (Translate 90 0) (verificaNumero (mod (div pt 10) 10) est)
   where pt = (pontos(jogador(jogo est)))

desenhaPontosNum5 :: Estado -> [Picture]
desenhaPontosNum5 est = map (Translate 120 0) (verificaNumero (mod pt 10) est)
   where pt = (pontos(jogador(jogo est)))

verificaNumero :: Int -> Estado -> [Picture]
verificaNumero int est | int == 0 = desenhaPontosAux est Num0
                       | int == 1 = desenhaPontosAux est Num1
                       | int == 2 = desenhaPontosAux est Num2
                       | int == 3 = desenhaPontosAux est Num3
                       | int == 4 = desenhaPontosAux est Num4
                       | int == 5 = desenhaPontosAux est Num5
                       | int == 6 = desenhaPontosAux est Num6
                       | int == 7 = desenhaPontosAux est Num7
                       | int == 8 = desenhaPontosAux est Num8
                       | int == 9 = desenhaPontosAux est Num9

desenhaPontosAux :: Estado -> Imagem -> [Picture]
desenhaPontosAux est img = [Translate (-690) (400) (Scale 0.05 0.05 ((getImagem img (imagens est))))]



--verifica se a casa das unidades de um numero é par
ePar :: Float -> Bool
ePar n = eParAux (mod' n 10)

eParAux :: Float -> Bool
eParAux n | n >= 0 && n < 1 = True 
          | n < 0 = False
          | otherwise = eParAux (n-2)


reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

reageTempo :: Float -> Estado -> Estado
--reageTempo t (Estado {jogo = (Jogo {mapa = m, inimigos = inim, colecionaveis =col, jogador = jog}), imagens = imgs,tempo=tp}) = (Estado {jogo = (Jogo {mapa = m, inimigos = inim, colecionaveis =col, jogador = jog {posicao = b (posicao jog)}}), imagens = imgs,tempo=tp+0.2})
--reageTempo _ s = s
reageTempo t s = s {tempo = tempo s + 0.2}

{-
b :: Posicao -> Posicao
b (x,y) = (x+0.1,y)
-}

