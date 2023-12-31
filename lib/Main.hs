module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import LI12324
import Tarefa2
import Tarefa3
import Tarefa5
import Imagens
import Mapa
import Data.Fixed


{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}

-- | Função principal, responsável por carregar os elementos visuais presentes no ambiente gŕafico do jogo
main :: IO ()
main = do
  images <- getImages
  play janela
       bg
       fr
       (estadoInicial images)
       desenhaEstado
       keys
       reageTempo

-- | Define as propriedades de tamanho o localização da janela do jogo
janela :: Display
janela = InWindow
  "Mapa"     -- título de janela    
  (1484,954)  -- dimensão da janela // 53*53 cada bloco
  (255,60)    -- posição no ecrã

-- | Define a cor de fundo da janela de jogo
bg :: Color
bg = black

-- | Define o número de frames por segundo, ou seja o número de vezes que o programa é atualizado por segundo
fr :: Int
fr = 20

-- | Recebe as imagens e devolve o estado inicial do jogo
estadoInicial :: Imagens -> Estado
estadoInicial images = Estado {jogo = j1, imagens = images, tempo = 0 ,bonus = 15000}

-- | Desenha no ecrã o que está a acontecer no jogo em cada momento
desenhaEstado :: Estado -> Picture
desenhaEstado s = Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)

-- | Fornece a lista de pictures (com as devidas translações) utilizadas para desenhar o mapa do jogo
desenhaMapa1 :: (Float,Float) -> Estado -> [Picture]
desenhaMapa1 _ (Estado {jogo = (Jogo {mapa = Mapa a b []})}) = []
desenhaMapa1 (x,y) (Estado {jogo = (Jogo {mapa = Mapa a b (h:t)}), imagens = imgs}) = (desenhaLinhas1 (x,y) imgs h) ++ (desenhaMapa1 (x,y-53) (Estado {jogo = (Jogo {mapa = Mapa a b t}), imagens = imgs}))

-- | Fornece a lista de pictures (com as devidas translações) utilizadas para desenhar um linha do mapa do jogo
desenhaLinhas1 :: (Float,Float) -> Imagens -> [Bloco] -> [Picture] 
desenhaLinhas1 _ _ [] = []
desenhaLinhas1 (x,y) imgs (h : t) | h == Escada = (Translate x y (getImagem Ladder imgs)) : desenhaLinhas1 (x+53,y) imgs t 
                                  | h == Plataforma = (Translate x y(getImagem Platform imgs)): desenhaLinhas1 (x+53,y) imgs t 
                                  | h == Alcapao = (Translate x y(getImagem Trapdoor imgs)): desenhaLinhas1 (x+53,y) imgs t 
                                  | h == Vazio = desenhaLinhas1 (x+53,y) imgs t 

-- | Fornece uma lista com um único elemento,sendo esse elemento uma picture do jogador (tendo em conta que ações ele está a realizar e o local do mapa onde se localiza)
desenhaJogador :: Estado -> [Picture]
desenhaJogador est | direcao (jogador (jogo est)) == Este = desenhaJogEste est
                   | direcao (jogador (jogo est)) == Oeste = desenhaJogOeste est
                   | otherwise = desenhaJogNorteSul est
                  
-- | Fornece uma lista com um único elemento, sendo esse elemento uma picture do jogador (é chamada apenas quando o jogador tem direção igual a Norte ou Sul, a imagem fornecida depende do tempo atual do estado)
desenhaJogNorteSul :: Estado -> [Picture]
desenhaJogNorteSul est | emEscada (jogador (jogo est)) == True && velocidadeJog /= (0,0) = if alteraImagem (realToFrac(tempo est))
                                                                                             then [desenhaJogadorAux est MarioClimbing1]
                                                                                             else [desenhaJogadorAux est MarioClimbing2]
                       | emEscada (jogador (jogo est)) == True && velocidadeJog == (0,0) = [desenhaJogadorAux est MarioClimbing1]
                       | otherwise = [desenhaJogadorAux est MarioStandingBack]
    where velocidadeJog = velocidade (jogador (jogo est))

-- | Fornece uma lista com um único elemento, sendo esse elemento uma picture do jogador (é chamada apenas quando o jogador tem direção igual a Este, a imagem fornecida depende do tempo atual do estado)
desenhaJogEste :: Estado -> [Picture]
desenhaJogEste est | snd velocidadeJog /= 0 = [desenhaJogadorAux est MarioJumpingRight1] -- está a saltar/cair para a direita
                   | velocidadeJog /= (0,0) && aplicaDanoJog == True = if alteraImagem (realToFrac(tempo est))
                                                                         then [Translate 20 0 (desenhaJogadorAux est MarioHammerRight1)] -- está a andar para a direita com o martelo para baixo
                                                                         else [Translate 0 20 (desenhaJogadorAux est MarioHammerRight4)] -- está a andar para a direita com o martelo para cima
                   | velocidadeJog /= (0,0) = if alteraImagem (realToFrac(tempo est))
                                                then [desenhaJogadorAux est MarioWalkingRight1] -- está a andar para a direita (desenho a andar)
                                                else [desenhaJogadorAux est MarioStandingRight] -- está a andar para a direita (desenho parado)
                   | velocidadeJog == (0,0) && aplicaDanoJog == True = if alteraImagem(realToFrac(tempo est))
                                                                         then [Translate 20 0 (desenhaJogadorAux est MarioHammerRight1)] --está parado e com martelo para baixo
                                                                         else [Translate 0 20 (desenhaJogadorAux est MarioHammerRight2)] --está parado e com martelo para cima
                   | otherwise = [desenhaJogadorAux est MarioStandingRight] -- mario parado virado pra direita
    where velocidadeJog = velocidade (jogador (jogo est))
          aplicaDanoJog = fst (aplicaDano(jogador(jogo est)))

-- | Fornece uma lista com um único elemento, sendo esse elemento uma picture do jogador (é chamada apenas quando o jogador tem direção igual a Oeste, a imagem fornecida depende do tempo atual do estado)
desenhaJogOeste :: Estado -> [Picture]
desenhaJogOeste est | snd velocidadeJog /= 0 = [desenhaJogadorAux est MarioJumpingLeft1] -- está a saltar/cair para a esquerda
                    | velocidadeJog /= (0,0) && aplicaDanoJog == True = if alteraImagem(realToFrac(tempo est))
                                                                          then [Translate (-20) 0 (desenhaJogadorAux est MarioHammerLeft1)] -- está a andar para a esquerda com o martelo para baixo
                                                                          else [Translate 0 20(desenhaJogadorAux est MarioHammerLeft4)] -- está a andar para a esquerda com o martelo para cima
                    | velocidadeJog /= (0,0) = if alteraImagem (realToFrac(tempo est))
                                                then [desenhaJogadorAux est MarioWalkingLeft1] -- está a andar para a esquerda (desenho a andar)
                                                else [desenhaJogadorAux est MarioStandingLeft] -- está a andar para a esquerda (desenho parado)
                    | velocidadeJog == (0,0) && aplicaDanoJog == True = if alteraImagem(realToFrac(tempo est)) 
                                                                          then [Translate (-20) 0 (desenhaJogadorAux est MarioHammerLeft1)] --está parado virado para a esquerda e com martelo para baixo
                                                                          else [Translate 0 20 (desenhaJogadorAux est MarioHammerLeft2)] --está parado virado para a esquerda e com martelo para cima
                    | otherwise = [desenhaJogadorAux est MarioStandingLeft] -- mario parado virado pra esquerda
    where velocidadeJog = velocidade (jogador (jogo est))
          aplicaDanoJog = fst (aplicaDano(jogador(jogo est)))

-- | Devolve uma picture com as devidas translações para se localizar na posição atual do jogador
desenhaJogadorAux :: Estado -> Imagem -> Picture
desenhaJogadorAux est img = Translate (x - 742) (477 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (posicao (jogador(jogo est)))) * 53
          y = realToFrac $ (snd (posicao (jogador(jogo est)))) * 53

-- | Fornece uma lista de pictures (com as devidas translações) utilizadas para desenhar os inimigos nas suas posições atuais tendo em conta a sua direção (só desenha o inimigo se este ainda tiver vidas restantes)
desenhaFantasmas :: Estado -> [Picture]
desenhaFantasmas (Estado {jogo = Jogo {inimigos = []}, imagens = imgs, tempo = tp}) = []
desenhaFantasmas (Estado {jogo = jog, imagens = imgs, tempo = tp}) | vida (head (inimigos jog)) > 0  && tipo (head (inimigos jog)) == Fantasma = desenhaFantasmasAux (Estado {jogo = jog {inimigos = take 1 (inimigos jog)}, imagens = imgs, tempo = tp}) : (desenhaFantasmas (Estado {jogo = jog {inimigos = drop 1 (inimigos jog)}, imagens = imgs, tempo = tp}))
                                                                   | otherwise = desenhaFantasmas (Estado {jogo = jog {inimigos = drop 1 (inimigos jog)}, imagens = imgs, tempo = tp})

-- | Fornece uma picture do inimigo com as devidas translações para esta estar na posição atual do inimigo (a imagem fornecida depende o tempo atual do estado)
desenhaFantasmasAux :: Estado -> Picture
desenhaFantasmasAux est | direcao (head (inimigos (jogo est))) == Este = if alteraImagem (realToFrac(tempo est))
                                                                          then desenhaFantAux est GhostRight1
                                                                          else desenhaFantAux est GhostRight2
                        | otherwise = if alteraImagem (realToFrac(tempo est))
                                       then desenhaFantAux est GhostLeft1
                                       else desenhaFantAux est GhostLeft2

-- | Fornece uma picture com as devidas translações para se localizar na posição atual do inimigo em questão 
desenhaFantAux :: Estado -> Imagem -> Picture
desenhaFantAux est img = Translate (x - 742) (480 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (posicao (head (inimigos(jogo est))))) * 53
          y = realToFrac $ (snd (posicao (head (inimigos(jogo est))))) * 53

desenhaMacacoMalvado :: Estado -> [Picture]
desenhaMacacoMalvado est | inimigos (jogo est) == [] = []
                         | entd == MacacoMalvado && vx == 0 = if alteraImagem (realToFrac (tempo est))
                                                                then [desenhaMacacoAux est MonkeyArmRight]
                                                                else [desenhaMacacoAux est MonkeyArmLeft]
                         | entd == MacacoMalvado && vx > 0 = [desenhaMacacoAux est MonkeyWalkingRight]
                         | entd == MacacoMalvado && vx < 0 = [desenhaMacacoAux est MonkeyWalkingLeft]
                         | otherwise = desenhaMacacoMalvado $ est {jogo = Jogo {inimigos = drop 1 (inimigos (jogo est))}}
  where (vx,vy) = velocidade (head(inimigos(jogo(est))))
        entd = tipo(head(inimigos(jogo est)))


desenhaMacacoAux :: Estado -> Imagem -> Picture
desenhaMacacoAux est img = Translate (x - 742) (477 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (posicao (head (inimigos(jogo est))))) * 53
          y = realToFrac $ (snd (posicao (head (inimigos(jogo est))))) * 53


-- | Fornece uma lista de pictures (com as devidas translações) utilizadas para desenhar o colecionáveis 
desenhaColecionaveis :: Estado -> [Picture]
desenhaColecionaveis (Estado {jogo = Jogo {colecionaveis= []}, imagens = imgs, tempo = tp}) = []
desenhaColecionaveis (Estado {jogo = jog, imagens = imgs, tempo = tp}) = desenhaColecionaveisAux (Estado {jogo = jog {colecionaveis = take 1 (colecionaveis jog)}, imagens = imgs, tempo = tp}) : (desenhaColecionaveis (Estado {jogo = jog {colecionaveis = drop 1 (colecionaveis jog)}, imagens = imgs, tempo = tp}))

-- | Fornece uma picture (com as devidas translações) utilizada para desenhar um colecionável
desenhaColecionaveisAux :: Estado -> Picture
desenhaColecionaveisAux est | fst (head (colecionaveis (jogo est))) == Moeda = desenhaColecAux est Coin
                            | otherwise = desenhaColecAux est Hammer 

-- | Fornece uma picture com as devidas translações para se localizar na posição do colecionável
desenhaColecAux :: Estado -> Imagem -> Picture
desenhaColecAux est img = Translate (x - 742) (477 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (snd (head (colecionaveis (jogo est))))) * 53
          y = realToFrac $ (snd (snd (head (colecionaveis (jogo est))))) * 53

-- | Fornece uma lista de um elemento, sendo esse elemento uma picture da estrela, ponto de chegada do jogo
desenhaEstrela :: Estado -> [Picture]
desenhaEstrela s = [Translate (x - 742) (477 - y) (getImagem Estrela (imagens s))]
  where x = 14 * 53
        y = 1.5 * 53

-- | Fornece uma lista de um elemento, sendo esse elemento uma picture dos corações (número de vidas) que o jogador tem
desenhaVida :: Estado -> [Picture] 
desenhaVida s | vida (jogador (jogo s)) == 0 = [Translate (-630) 340 (Scale 0.3 0.3 (getImagem ZeroVidas (imagens s)))]
              | vida (jogador (jogo s)) == 1 = [Translate (-630) 340 (Scale 0.3 0.3 (getImagem UmaVida (imagens s)))]
              | vida (jogador (jogo s)) == 2 = [Translate (-630) 340 (Scale 0.3 0.3 (getImagem DuasVidas (imagens s)))]
              | otherwise = [Translate (-630) 340 (Scale 0.3 0.3 ( getImagem TresVidas (imagens s)))]

-- | Fornece uma lista de pictures utilizadas para desenhar a pontuação do jogador no ecrã
desenhaPontos :: Estado -> [Picture]
desenhaPontos est = desenhaPontosImg est ++ desenhaPontosNum est

-- | Fornece uma lista de um elemento, sendo esse elemento uma picture do quadrado onde irá aparecer a pontuação atual do jogador
desenhaPontosImg :: Estado -> [Picture]
desenhaPontosImg est = [Translate (-631) (420) (getImagem Score (imagens est))]

-- | Fornece uma lista de pictures com as devidas translações para desenhar a pontuação atual do Jogador no ecrã
desenhaPontosNum :: Estado -> [Picture]
desenhaPontosNum est = desenhaPontosNum1 est ++ desenhaPontosNum2 est ++ desenhaPontosNum3 est++ desenhaPontosNum4 est++ desenhaPontosNum5 est

-- | Fornece uma lista de um elemento, sendo esse elemento a picture relativa ao primeiro algarismo da pontuação atual do jogador
desenhaPontosNum1 :: Estado -> [Picture]
desenhaPontosNum1 est = verificaNumero (div pt 10000) est
   where pt = (pontos(jogador(jogo est)))

-- | Fornece uma lista de um elemento, sendo esse elemento a picture relativa ao segundo algarismo da pontuação atual do jogador
desenhaPontosNum2 :: Estado -> [Picture]
desenhaPontosNum2 est = map (Translate 30 0) (verificaNumero (mod (div pt 1000) 10) est)
   where pt = (pontos(jogador(jogo est)))

-- | Fornece uma lista de um elemento, sendo esse elemento a picture relativa ao terceiro algarismo da pontuação atual do jogador
desenhaPontosNum3 :: Estado -> [Picture]
desenhaPontosNum3 est = map (Translate 60 0) (verificaNumero (mod (div pt 100) 10) est)
   where pt = (pontos(jogador(jogo est)))

-- | Fornece uma lista de um elemento, sendo esse elemento a picture relativa ao quarto algarismo da pontuação atual do jogador
desenhaPontosNum4 :: Estado -> [Picture]
desenhaPontosNum4 est = map (Translate 90 0) (verificaNumero (mod (div pt 10) 10) est)
   where pt = (pontos(jogador(jogo est)))

-- | Fornece uma lista de um elemento, sendo esse elemento a picture relativa ao quinto algarismo da pontuação atual do jogador
desenhaPontosNum5 :: Estado -> [Picture]
desenhaPontosNum5 est = map (Translate 120 0) (verificaNumero (mod pt 10) est)
   where pt = (pontos(jogador(jogo est)))

-- | Fornece uma lista de um elemento, sendo esse elemento uma picture relativa ao número da pontuação que o jogador tem em dada casa 
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
                       | otherwise = desenhaPontosAux est Num9

-- | Pega na imagem recebida e transforma-a numa picture com escala e translação adequadas ao pretendido
desenhaPontosAux :: Estado -> Imagem -> [Picture]
desenhaPontosAux est img = [Translate (-690) (400) (Scale 0.05 0.05 ((getImagem img (imagens est))))]

-- | Fornece uma lista de pictures utilizadas para desenhar o bonus no ecrã do jogo
desenhaBonus :: Estado -> [Picture]--tira 5 a cada reage tempo, logo tira 100 por segundo, acada ao fim de 2min30s
desenhaBonus s = desenhaBonusImg s ++ desenhaBonusNum s

-- | Fornece uma lista de um único elemento, sendo esse elemento uma picture do quadrado onde irá aparecer o Bonus atual
desenhaBonusImg :: Estado -> [Picture]
desenhaBonusImg s = [Translate (630) (418) (getImagem Bonus (imagens s))]

-- | Fornece uma lista de pictures com as devidas translações utilizadas para desenhar o bonus atual no ecrã
desenhaBonusNum :: Estado -> [Picture]
desenhaBonusNum s = desenhaBonusNum1 s ++ desenhaBonusNum2 s ++ desenhaBonusNum3 s ++ desenhaBonusNum4 s ++ desenhaBonusNum5 s

-- | Fornece uma lista de um único elemento, sendo esse elemento uma picture relativa ao primeiro algarismo do bonus atual
desenhaBonusNum1 :: Estado -> [Picture]
desenhaBonusNum1 est = map (Translate 1265 0) (verificaNumero (div (bonus est) 10000) est)

-- | Fornece uma lista de um único elemento, sendo esse elemento uma picture relativa ao segundo algarismo do bonus atual
desenhaBonusNum2 :: Estado -> [Picture]
desenhaBonusNum2 est = map (Translate 1295 0) (verificaNumero (mod (div (bonus est) 1000) 10) est)

-- | Fornece uma lista de um único elemento, sendo esse elemento uma picture relativa ao terceiro algarismo do bonus atual
desenhaBonusNum3 :: Estado -> [Picture]
desenhaBonusNum3 est = map (Translate 1325 0) (verificaNumero (mod (div (bonus est) 100) 10) est)

-- | Fornece uma lista de um único elemento, sendo esse elemento uma picture relativa ao quarto algarismo do bonus atual
desenhaBonusNum4 :: Estado -> [Picture]
desenhaBonusNum4 est = map (Translate 1355 0) (desenhaPontosAux est Num0)

-- | Fornece uma lista de um único elemento, sendo esse elemento uma picture relativa ao quinto algarismo do bonus atual
desenhaBonusNum5 :: Estado -> [Picture]
desenhaBonusNum5 est = map (Translate 1385 0) (desenhaPontosAux est Num0) --map (Translate 1385 0) (verificaNumero (mod (bonus est) 10) est)


-- | Verifica se a parte decimal de um número está entre 0 e 25 ou 50 e 75
alteraImagem :: Float -> Bool
alteraImagem n = alteraImagemAux (mod' (n * 10) 10)

-- | Verifica se um número está entre 0 e 2.5 ou 5 e 7.5
alteraImagemAux :: Float -> Bool
alteraImagemAux n = n >= 0 && n<2.5 || n>=5 && n<7.5


reageEvento :: Event -> Estado -> Estado
reageEvento _ s = s

reageTempo :: Float -> Estado -> Estado
--reageTempo t s = s {tempo = tempo s + 0.05}
reageTempo t s = s {jogo = movimenta 4 (realToFrac t) (jogo s),tempo = tempo s + (realToFrac t), bonus = (bonus s) - 5 }



