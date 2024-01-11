module Main where
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game
import Graphics.Gloss.Interface.IO.Game
import LI12324
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa5
import Imagens
import Mapa
import Music
import Data.Fixed
import Data.List


{- main :: IO ()
main = do
  putStrLn "Hello, PrimateKong!"
-}





-- | Função principal, responsável por carregar os elementos visuais presentes no ambiente gŕafico do jogo
main :: IO ()
main = do
  musicaMenu
  images <- getImages
  playIO janela
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
estadoInicial images = Estado {menu = Inicio,jogo = jOpcoes, imagens = images, tempo = 0 ,bonus = 15000, highScore = [(0,"XXXXX"), (0,"XXXXX"),(0,"XXXXX")], saltar = 2, editor = False}

-- | Desenha no ecrã o que está a acontecer no jogo em cada momento
desenhaEstado :: Estado -> IO Picture
desenhaEstado s | menu s == Inicio              = return (Pictures(desenhaInicio s))
                | menu s == ModoJogo            = return (Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s++[Translate 0 460 (Scale 0.5 0.5 (getImagem PlPressP (imagens s)))]))
                | menu s == ModoPausa Continuar = return (Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s ++ [Scale 0.7 0.7 (getImagem Pausa4 (imagens s)), Translate 0 (-270) (getImagem DicasPausa (imagens s))]))
                | menu s == ModoPausa Reiniciar = return (Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s ++ [Scale 0.7 0.7 (getImagem Pausa1 (imagens s)), Translate 0 (-270) (getImagem DicasPausa (imagens s))]))
                | menu s == ModoPausa Home      = return (Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s ++ [Scale 0.7 0.7 (getImagem Pausa2 (imagens s)), Translate 0 (-270) (getImagem DicasPausa (imagens s))]))
                | menu s == ModoPausa Controls  = return (Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s ++ [Scale 0.7 0.7 (getImagem Pausa3 (imagens s)), Translate 0 (-270) (getImagem DicasPausa (imagens s))]))
                | menu s == ModoHighScore       = return (Pictures(desenhaModoHighScore s))
                | menu s == GanhouJogo          = return (Pictures(desenhaGanhouJogo s))
                | menu s == GanhouJogoEditor    = return (Pictures([Translate 0 (-30) (Scale 2.5 2.5 (getImagem MonkeyDefeated (imagens s))), Translate 0 300 (getImagem PlParabens (imagens s)),  Translate 0 180 (getImagem PlDerrotasteOPrimateKong (imagens s))] ++ map (Translate 0 (-150)) (map (Scale 2 2) (desenhaFogo s)) ++ map (Translate 80 (-260)) (desenhaScoreFinal s) ++ [Translate 70 (-275) (rectangleSolid 700 150)]))
                | menu s == PerdeuJogo          = return (Pictures(desenhaPerdeuJogo s (tempo s)))
                | menu s == ModoControlos       = return (Pictures((desenhaMapa1 (-715.5,450.5) s) ++ desenhaJogador s ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s ++ [Scale 0.7 0.7 (getImagem PausaControlos (imagens s)),Scale 0.15 0.15 (Translate 0 (-1670) (getImagem PlPressEnter (imagens s)))]))
                | menu s == Editor1             = return (Pictures(desenhaEditor1 s))
                | menu s == Editor2             = return (Pictures(desenhaEditor2 s))
                | otherwise                     = return (Pictures(desenhaOpcoes s))

-- | Fornece uma lista de pictures utilizadas para desenhar o ecrã inicial do jogo
desenhaInicio :: Estado -> [Picture]
desenhaInicio s | alteraImagem2 (realToFrac (tempo s)) = [Translate 0 (-200) (Scale 0.3 0.3 (getImagem PlPressEnter (imagens s))),getImagem PrimateKong (imagens s)]
                | otherwise = [getImagem PrimateKong (imagens s)]

-- | Fornece uma lista de pictures utilizadas para desenhar o ecrã das opções disponiveis ao jogador
desenhaOpcoes :: Estado -> [Picture]
desenhaOpcoes s = desenhaOpcoesFundo s ++ desenhaOpcoesOpcao s

-- | Fornece uma lista de pictures utilizadas para desenhar o jogo que ocorre no fundo do ecrã de opções disponiveis ao jogador
desenhaOpcoesFundo :: Estado -> [Picture]
desenhaOpcoesFundo s = (desenhaMapa1 (-715.5,450.5) s ++ desenhaFantasmas s ++ desenhaMacacoOpcoes s)

-- | Fornece uma lista de pictures utlizadas para desenhar as opções disponiveis, tendo em conta a opção que está selecionada atualmente
desenhaOpcoesOpcao :: Estado -> [Picture]
desenhaOpcoesOpcao s | menu s == Opcoes Jogar = [Scale 1.4 1.4 (getImagem PalavraJogar (imagens s))] ++ [Translate 0 (-80) (getImagem PalavraHighScore (imagens s))] ++ [Translate (-150) 0 (getImagem MarioStandingRight (imagens s))] ++ [Translate 150 0 (getImagem MarioStandingLeft (imagens s))] ++ [Scale 0.7 0.7 (Translate 0 (-200) (getImagem Creditos (imagens s)))] ++ [Translate 0 (-205) (getImagem PlCriaMapa (imagens s))] 
                     | menu s == Opcoes HighScore = [getImagem PalavraJogar (imagens s)] ++ [Translate 0 (-70) (Scale 1.2 1.2 (getImagem PalavraHighScore (imagens s)))] ++ [Translate (-210) (-65) (getImagem MarioStandingRight (imagens s))] ++ [Translate 210 (-70) (getImagem MarioStandingLeft (imagens s))] ++ [Scale 0.7 0.7 (Translate 0 (-200) (getImagem Creditos (imagens s)))] ++ [Translate 0 (-205) (getImagem PlCriaMapa (imagens s))] 
                     | menu s == Opcoes OpCreditos = [(getImagem PalavraJogar (imagens s))] ++ [Translate 0 (-80) (getImagem PalavraHighScore (imagens s))] ++ [Translate (-210) (-150) (getImagem MarioStandingRight (imagens s))] ++ [Translate 210 (-150) (getImagem MarioStandingLeft (imagens s))] ++ [Scale 1 1 (Translate 0 (-140) (getImagem Creditos (imagens s)))] ++ [Translate 0 (-205) (getImagem PlCriaMapa (imagens s))] 
                     | menu s == Opcoes EditorMapas =[getImagem PalavraJogar (imagens s)] ++ [Translate 0 (-80) (getImagem PalavraHighScore (imagens s))] ++ [Translate (-300) (-200) (getImagem MarioStandingRight (imagens s))] ++ [Translate 300 (-200) (getImagem MarioStandingLeft (imagens s))] ++ [Scale 0.7 0.7 (Translate 0 (-200) (getImagem Creditos (imagens s)))] ++ [Scale 1.15 1.15 (Translate 0 (-180) (getImagem PlCriaMapa (imagens s)))] 
                     | otherwise = [rectangleSolid 50 50]

-- | Fornece uma lista de pictures de um único elemento que consiste numa imagem do MacacoMalvado situado no ecrã de opções (utilizando o tempo do estado para criar movimento no mesmo)
desenhaMacacoOpcoes :: Estado -> [Picture]
desenhaMacacoOpcoes s | (t >= 0 && t<=3) || (t>=5 && t<=8) = [Translate 0 320 (Scale 2 2 (getImagem MonkeyStanding (imagens s)))]
                      | alteraImagem (realToFrac (tempo s)) = [Translate 0 320 (Scale 2 2 (getImagem MonkeyArmLeft (imagens s)))]
                      | otherwise = [Translate 0 320 (Scale 2 2 (getImagem MonkeyArmRight (imagens s)))]
   where t = mod' (tempo s) 10

-- | Fornece a lista de pictures (com as devidas translações) utilizadas para desenhar o mapa do jogo
desenhaMapa1 :: (Float,Float) -> Estado -> [Picture]
desenhaMapa1 _ (Estado {jogo = (Jogo {mapa = Mapa a b []})}) = []
desenhaMapa1 (x,y) s = (desenhaLinhas1 (x,y) (imagens s) h) ++ (desenhaMapa1 (x,y-53)) (s{jogo = Jogo {mapa = Mapa a b t, inimigos = inimigos (jogo s), colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}})
  where Mapa a b (h:t) = mapa (jogo s)

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
desenhaFantasmas (Estado {jogo = jog, imagens = imgs, tempo = tp}) = map (desenhaFantasmasAux imgs tp (jogador jog)) (inimigos jog)

-- | Fornece um picture que é utilizada para criar o efeito de explosão quando o fantasma é atingido pelo martelo
desenhaFantasmasAux :: Imagens -> Tempo -> Personagem -> Personagem -> Picture
desenhaFantasmasAux img t jog inim | tipo inim == Fantasma && vida inim == 1 = desenhaFantasmaVivo img t jog inim
                                   | tipo inim == Fantasma && vida inim == 0 = desenhaFantAux inim img GhostDefeated1
                                   | tipo inim == Fantasma && vida inim >4 && vida inim <=6 = desenhaFantAux inim img GhostDefeated2
                                   | tipo inim == Fantasma && vida inim >6 && vida inim <= 8 = desenhaFantAux inim img GhostDefeated3
                                   | tipo inim == Fantasma && vida inim >8 && vida inim <= 10 = desenhaFantAux inim img GhostDefeated4
                                   | otherwise = rectangleSolid 0.1 0.1

-- | Fornece uma picture do inimigo com as devidas translações para esta estar na posição atual do inimigo (a imagem fornecida depende o tempo atual do estado)
desenhaFantasmaVivo :: Imagens -> Tempo -> Personagem -> Personagem -> Picture
desenhaFantasmaVivo img t jog inim | direcao inim == Este && fst (aplicaDano jog) == False = if alteraImagem (realToFrac t)
                                                                                                then desenhaFantAux inim img GhostRight1
                                                                                                else desenhaFantAux inim img GhostRight2
                                   | direcao inim == Este = if alteraImagem (realToFrac t)
                                                              then desenhaFantAux inim img GhostBlueRight1
                                                              else desenhaFantAux inim img GhostBlueRight2
                                   | direcao inim == Oeste && fst (aplicaDano jog) == False = if alteraImagem (realToFrac t)
                                                                                                 then desenhaFantAux inim img GhostLeft1
                                                                                                 else desenhaFantAux inim img GhostLeft2
                                   | otherwise = if alteraImagem (realToFrac t)
                                                   then desenhaFantAux inim img GhostBlueLeft1
                                                   else desenhaFantAux inim img GhostBlueLeft2

-- | Fornece uma picture com as devidas translações para se localizar na posição atual do inimigo em questão 
desenhaFantAux :: Personagem -> Imagens -> Imagem -> Picture
desenhaFantAux inim imags img = Translate (x - 742) (480 - y) (getImagem img imags)
    where x = realToFrac $ (fst (posicao inim)) * 53
          y = realToFrac $ (snd (posicao inim)) * 53

-- | Fornece uma lista de pictures de um único elemento sendo este uma picture do MacacoMalvado (tendo em conta a ação que este está a realizar no momento)
desenhaMacacoMalvado :: Estado -> [Picture]
desenhaMacacoMalvado est | inimigos (jogo est) == [] = []
                         | entd == MacacoMalvado && y > 16.2 = [Translate 0 (-3) (desenhaMacacoAux est MonkeyDefeated)]
                         | entd == MacacoMalvado && vid == 11 = [desenhaMacacoAux est MonkeyFalling]
                         | entd == MacacoMalvado && vx == 0 = if alteraImagem (realToFrac (tempo est))
                                                                then [desenhaMacacoAux est MonkeyArmRight]
                                                                else [desenhaMacacoAux est MonkeyArmLeft]
                         | entd == MacacoMalvado && vx > 0 = [desenhaMacacoAux est MonkeyWalkingRight]
                         | entd == MacacoMalvado && vx < 0 = [desenhaMacacoAux est MonkeyWalkingLeft]             
                         | otherwise = desenhaMacacoMalvado $ est {jogo = Jogo {mapa = mapa (jogo est), inimigos = drop 1 (inimigos (jogo est)), colecionaveis = colecionaveis (jogo est), jogador = jogador (jogo est)}}
  where (vx,vy) = velocidade (head(inimigos(jogo(est))))
        entd = tipo(head(inimigos(jogo est)))
        vid = vida (head (inimigos (jogo est)))
        y = snd (posicao(head(inimigos(jogo est))))

-- | Fornece uma Picture com as devidas translações utilizada para desenhar o MacacoMalvado no local da sua posição
desenhaMacacoAux :: Estado -> Imagem -> Picture
desenhaMacacoAux est img = Translate (x - 742) (477 - y) (getImagem img (imagens est))
    where x = realToFrac $ (fst (posicao (head (inimigos(jogo est))))) * 53
          y = realToFrac $ (snd (posicao (head (inimigos(jogo est))))) * 53


-- | Fornece uma lista de pictures (com as devidas translações) utilizadas para desenhar o colecionáveis 
desenhaColecionaveis :: Estado -> [Picture]
desenhaColecionaveis (Estado {jogo = Jogo {colecionaveis= []}}) = []
desenhaColecionaveis s = desenhaColecionaveisAux (s {jogo = jog {mapa = mapa (jogo s), inimigos = inimigos (jogo s), colecionaveis = take 1 (colecionaveis jog), jogador = jogador (jogo s)}}) : (desenhaColecionaveis (s {jogo = jog {mapa = mapa (jogo s), inimigos = inimigos (jogo s), colecionaveis = drop 1 (colecionaveis jog), jogador = jogador (jogo s)}}))
  where jog = jogo s
        imgs = imagens s
        tp = tempo s

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
desenhaEstrela s = [Translate (realToFrac (x*53 - 742)) (realToFrac (477 - y*53)) (getImagem Estrela (imagens s))]
  where Mapa a (x,y) b = mapa(jogo s)

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

-- | Fornece uma lista de pictures utilizadas para desenhar o ecrã de vitória do jogo principal
desenhaGanhouJogo :: Estado -> [Picture]
desenhaGanhouJogo s = desenhaMapa1 (-715.5,450.5) s ++ desenhaJogador s ++ desenhaMacacoMalvado s ++ desenhaBonus s ++ desenhaVida s ++ desenhaPontos s ++ desenhaFogo s ++ desenhaParabens s ++ desenhaScoreFinal s ++ desenhaNome (snd (last (highScore s))) (imagens s) 130

-- | Fornece uma lista de pictures utilizadas para desenhar o fogo de artifico presente na animação de vitória do jogo
desenhaFogo :: Estado -> [Picture]
desenhaFogo s | t>=3.00 && t <3.15 = [trans1 f1]
              | t>=3.15 && t <3.30 = [trans1 f2]
              | t>=3.30 && t <3.45 = [trans1 f3,trans2 f1]
              | t>=3.45 && t <3.60 = [trans1 f4,trans2 f2]
              | t>=3.60 && t <3.75 = [trans1 f5,trans2 f3,trans3 f1]
              | t>=3.75 && t <3.90 = [trans1 f6,trans2 f4,trans3 f2]
              | t>=3.90 && t <4.05 = [trans1 f7,trans2 f5,trans3 f3,trans4 f1]
              | t>=4.05 && t <4.20 = [trans1 f8,trans2 f6,trans3 f4,trans4 f2]
              | t>=4.20 && t <4.35 = [trans1 f9,trans2 f7,trans3 f5,trans4 f3,trans5 f1]
              | t>=4.35 && t <4.50 = [trans1 f10,trans2 f8,trans3 f6,trans4 f4,trans5 f2]
              | t>=4.50 && t <4.65 = [trans2 f9,trans3 f7,trans4 f5,trans5 f3,trans6 f1]
              | t>=4.65 && t <4.80 = [trans2 f10,trans3 f8,trans4 f6,trans5 f4,trans6 f2]
              | t>=4.80 && t <4.95 = [trans3 f9,trans4 f7,trans5 f5,trans6 f3]
              | t>=4.95 && t <5.10 = [trans3 f10,trans4 f8,trans5 f6,trans6 f4]
              | t>=5.10 && t <5.25 = [trans4 f9,trans5 f7,trans6 f5]
              | t>=5.25 && t <5.40 = [trans4 f10,trans5 f8,trans6 f6]
              | t>=5.40 && t <5.55 = [trans5 f9,trans6 f7]
              | t>=5.55 && t <5.70 = [trans5 f10,trans6 f8]
              | t>=5.70 && t <5.85 = [trans6 f9]
              | t>=5.85 && t <6.00 = [trans6 f10]
              | otherwise = []
   where t = tempo s
         trans1 = Translate 300 260
         trans2 = Translate (-300) 220
         trans3 = Translate 30 130
         trans4 = Translate 300 160
         trans5 = Translate (-100) 290
         trans6 = Translate (-180) 130
         f1 = getImagem Firework1 (imagens s)
         f2 = getImagem Firework2 (imagens s)
         f3 = getImagem Firework3 (imagens s)
         f4 = getImagem Firework4 (imagens s)
         f5 = getImagem Firework5 (imagens s)
         f6 = getImagem Firework6 (imagens s)
         f7 = getImagem Firework7 (imagens s)
         f8 = getImagem Firework8 (imagens s)
         f9 = getImagem Firework9 (imagens s)
         f10 = getImagem Firework10 (imagens s)

-- | Fornece uma lista de pictures utlizadas para desenhar a mensagem de parabéns ao jogador quando este ganha o nível
desenhaParabens :: Estado -> [Picture]
desenhaParabens s | tempo s >= 3 = [Translate 0 250 (Scale 0.5 0.5 (getImagem PlParabens (imagens s))), Translate 0 170 (Scale 1 1 (getImagem PlDerrotasteOPrimateKong (imagens s)))]
                  | otherwise = []

-- | Fornece uma lista de pictures utilizadas para desenhar o Score final do jogador, o HighScore atual e o nome do jogador atual
desenhaScoreFinal :: Estado -> [Picture]
desenhaScoreFinal s | tempo s > 11 =[Translate (-160) 80 (getImagem PlTeuScore (imagens s)), Translate (-97) 40 (getImagem PlHighScoreAtual (imagens s)), Translate (-76) (0) (getImagem PlEscreveNome (imagens s)),color white( line [(115,(-15)),(330,(-15))])] ++ map (Translate 680 (-317)) (desenhaPontosNum s) ++ map (Translate 795 (-357)) (desenhaHighScore s) ++ [Translate 0 (-50) (Scale 0.5 0.5 (getImagem PlPressEnter2 (imagens s)))]
                    | tempo s > 10 =[Translate (-160) 80 (getImagem PlTeuScore (imagens s)), Translate (-97) 40 (getImagem PlHighScoreAtual (imagens s)), Translate (-76) (0) (getImagem PlEscreveNome (imagens s)),color white( line [(115,(-15)),(330,(-15))])] ++ map (Translate 680 (-317)) (desenhaPontosNum s) ++ map (Translate 795 (-357)) (desenhaHighScore s)
                    | tempo s > 9 = [Translate (-160) 80 (getImagem PlTeuScore (imagens s)), Translate (-97) 40 (getImagem PlHighScoreAtual (imagens s))] ++ map (Translate 680 (-317)) (desenhaPontosNum s) ++ map (Translate 795 (-357)) (desenhaHighScore s)
                    | tempo s > 8 = [Translate (-160) 80 (getImagem PlTeuScore (imagens s)), Translate (-97) 40 (getImagem PlHighScoreAtual (imagens s))] ++ map (Translate 680 (-317)) (desenhaPontosNum s)
                    | tempo s > 7 = [Translate (-160) 80 (getImagem PlTeuScore (imagens s))] ++ map (Translate 680 (-317)) (desenhaPontosNum s)
                    | tempo s > 6 = [Translate (-160) 80 (getImagem PlTeuScore (imagens s))] 
                    | otherwise = []

-- | Fornece uma lista de pictures utilizadas para desenhar o HighScore atual
desenhaHighScore :: Estado -> [Picture]
desenhaHighScore est = desenhaHighScoreNum1 est ++ desenhaHighScoreNum2 est ++ desenhaHighScoreNum3 est++ desenhaHighScoreNum4 est++ desenhaHighScoreNum5 est

-- | Fornece uma lista de pictures de um único elemento, sendo esse elemento utilizado para desenhar o primeiro algarismo do HighScore atual
desenhaHighScoreNum1 :: Estado -> [Picture]
desenhaHighScoreNum1 est = verificaNumero (div pt 10000) est
   where pt = fst $ head $ reverse $ sort $ highScore est

-- | Fornece uma lista de pictures de um único elemento, sendo esse elemento utilizado para desenhar o segundo algarismo do HighScore atual
desenhaHighScoreNum2 :: Estado -> [Picture]
desenhaHighScoreNum2 est = map (Translate 30 0) (verificaNumero (mod (div pt 1000) 10) est)
   where pt = fst $ head $ reverse $ sort $ highScore est

-- | Fornece uma lista de pictures de um único elemento, sendo esse elemento utilizado para desenhar o terceiro algarismo do HighScore atual
desenhaHighScoreNum3 :: Estado -> [Picture]
desenhaHighScoreNum3 est = map (Translate 60 0) (verificaNumero (mod (div pt 100) 10) est)
   where pt = fst $ head $ reverse $ sort $ highScore est

-- | Fornece uma lista de pictures de um único elemento, sendo esse elemento utilizado para desenhar o quarto algarismo do HighScore atual
desenhaHighScoreNum4 :: Estado -> [Picture]
desenhaHighScoreNum4 est = map (Translate 90 0) (desenhaPontosAux est Num0)

-- | Fornece uma lista de pictures de um único elemento, sendo esse elemento utilizado para desenhar o quinto algarismo do HighScore atual
desenhaHighScoreNum5 :: Estado -> [Picture]
desenhaHighScoreNum5 est = map (Translate 120 0) (desenhaPontosAux est Num0)

-- | Fornece uma lista de pictures utilizadas para desenhar o nome dos jogadores
desenhaNome :: String -> Imagens -> Float -> [Picture] 
desenhaNome [] _ _ = []
desenhaNome (h:t) i n | h == 'A' = Translate n 0 (getImagem A i) :  desenhaNome t i (n+27)
                      | h == 'B' = Translate n 0 (getImagem B i) :  (desenhaNome t i (n+27))
                      | h == 'C' = Translate n 0 (getImagem C i) :  (desenhaNome t i (n+27))
                      | h == 'D' = Translate n 0 (getImagem D i) :  (desenhaNome t i (n+27))
                      | h == 'E' = Translate n 0 (getImagem E i) :  (desenhaNome t i (n+27))
                      | h == 'F' = Translate n 0 (getImagem F i) :  (desenhaNome t i (n+27))
                      | h == 'G' = Translate n 0 (getImagem G i) :  (desenhaNome t i (n+27))
                      | h == 'H' = Translate n 0 (getImagem H i) :  (desenhaNome t i (n+27))
                      | h == 'I' = Translate n 0 (getImagem I i) :  (desenhaNome t i (n+27))
                      | h == 'J' = Translate n 0 (getImagem J i) :  (desenhaNome t i (n+27))
                      | h == 'K' = Translate n 0 (getImagem K i) :  (desenhaNome t i (n+27))
                      | h == 'L' = Translate n 0 (getImagem L i) :  (desenhaNome t i (n+27))
                      | h == 'M' = Translate n 0 (getImagem M i) :  (desenhaNome t i (n+27))
                      | h == 'N' = Translate n 0 (getImagem N i) :  (desenhaNome t i (n+27))
                      | h == 'O' = Translate n 0 (getImagem O i) :  (desenhaNome t i (n+27))
                      | h == 'P' = Translate n 0 (getImagem P i) :  (desenhaNome t i (n+27))
                      | h == 'Q' = Translate n 0 (getImagem Q i) :  (desenhaNome t i (n+27))
                      | h == 'R' = Translate n 0 (getImagem R i) :  (desenhaNome t i (n+27))
                      | h == 'S' = Translate n 0 (getImagem S i) :  (desenhaNome t i (n+27))
                      | h == 'T' = Translate n 0 (getImagem T i) :  (desenhaNome t i (n+27))
                      | h == 'U' = Translate n 0 (getImagem U i) :  (desenhaNome t i (n+27))
                      | h == 'V' = Translate n 0 (getImagem V i) :  (desenhaNome t i (n+27))
                      | h == 'W' = Translate n 0 (getImagem W i) :  (desenhaNome t i (n+27))
                      | h == 'X' = Translate n 0 (getImagem X i) :  (desenhaNome t i (n+27))
                      | h == 'Y' = Translate n 0 (getImagem Y i) :  (desenhaNome t i (n+27))
                      | h == 'Z' = Translate n 0 (getImagem Z i) :  (desenhaNome t i (n+27))
desenhaNome _ _ _ = []

-- | Fornece uma lista de pictures utilizadas para desenhar a animação do jogador com 0 vidas e o ecrã final de game over
desenhaPerdeuJogo :: Estado -> Double -> [Picture]
desenhaPerdeuJogo s n | n >= 2.4 = [Translate 0 (-70) (Scale 2 2 (getImagem MarioDefeatedFinal (imagens s))), Translate 0 270 (getImagem Gameover (imagens s)), Translate 0 (-300) (getImagem PlPressEnter2 (imagens s))]
                      | n >= 2.1 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated4] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 1.8 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated3] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 1.5 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated2] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 1.2 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated1] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 0.9 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated4] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 0.6 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated3] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 0.3 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated2] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | n >= 0 = ((desenhaMapa1 (-715.5,450.5) s) ++ [desenhaJogadorAux s MarioDefeated1] ++ desenhaFantasmas s++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s ++ desenhaVida s ++ desenhaPontos s ++ desenhaBonus s)
                      | otherwise = []

-- | Fornece uma lista de pictures utilizadas para desenhar a tabela de HighScores 
desenhaModoHighScore :: Estado -> [Picture]
desenhaModoHighScore s = map (Translate 230 (66)) (desenhaNome (snd (head (highScore est))) (imagens s) 0) ++ 
                         map (Translate 350 (-330)) (desenhaHighScore est )++ 
                         map (Translate 230 (-4)) ((desenhaNome  (snd (head(drop 1 (highScore est))))) (imagens s) 0 )++ 
                         map (Translate 350 (-400)) (desenhaHighScore (retiraHighScore est))  ++ 
                         map (Translate 230 (-74)) ((desenhaNome  (snd (head(drop 2 (highScore est))))) (imagens s) 0 )++ 
                         map (Translate 350 (-470)) (desenhaHighScore (retiraHighScore (retiraHighScore est))) ++ 
                         [Scale 0.8 0.8 (Translate 0 300 (getImagem AmareloHighScore (imagens s))),Translate (-390) 70 (getImagem Ouro (imagens s)),Translate (-390) (0) (getImagem Prata (imagens s)),Translate (-390) (-70) (getImagem Bronze (imagens s)), Scale 0.32 0.32 (Translate 0 (-265) (getImagem Pontos (imagens s))), Scale 0.32 0.32 (Translate 0 (-30) (getImagem Pontos (imagens s))),Scale 0.32 0.32 (Translate 0 190 (getImagem Pontos (imagens s))), Translate 0 (-400) (getImagem PlPressEnter2 (imagens s))]
  where est = ordenaHighScore s

-- | Fornece uma lista de pictures utilizadas para desenhar a primeira fase do editor de mapas (edição da posição dos blocos, inimigos, colecionaveis e estrela)
desenhaEditor1 :: Estado -> [Picture]
desenhaEditor1 s = desenhaMapa1 (-715.5,450.5) s ++ desenhaQuadrado s ++ desenhaFantasmas s ++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s

-- | Fornece uma lista de pictures utilizadas para desenhar a segunda fase do editor de mapas (edição da posição do jogador)
desenhaEditor2 :: Estado -> [Picture]
desenhaEditor2 s = desenhaMapa1 (-715.5,450.5) s ++ desenhaJogador s ++ desenhaFantasmas s ++ desenhaMacacoMalvado s ++ desenhaColecionaveis s ++ desenhaEstrela s

-- | Fornece uma lista de pictures de um único elemento, sendo esse elemento o quadrado amarelo existente no editor de mapas
desenhaQuadrado :: Estado -> [Picture]
desenhaQuadrado s = [Translate (x-742) (477 - y) (getImagem Quadrado (imagens s))]
    where x = realToFrac $ (fst (posicao (jogador (jogo s)))) * 53
          y = realToFrac $ (snd (posicao (jogador (jogo s)))) * 53

-- | Ordena o a lista do HighScore do maior para o mais pequeno
ordenaHighScore :: Estado -> Estado
ordenaHighScore s = s{highScore = reverse $ sort $ highScore s}

-- | Retira o primeiro elemento da lista de HighScores
retiraHighScore :: Estado -> Estado 
retiraHighScore s = s {highScore = drop 1 (highScore s)}

-- | Verifica se a parte decimal de um número está entre 0 e 25 ou 50 e 75, utilizada para alterar uma imagem de 0,25 em 0,24 segundos
alteraImagem :: Float -> Bool
alteraImagem n = alteraImagemAux (mod' (n * 10) 10)

-- | Verifica se um número está entre 0 e 2.5 ou 5 e 7.5
alteraImagemAux :: Float -> Bool
alteraImagemAux n = (n >= 0 && n<2.5) || (n>=5 && n<7.5)

-- | True quando a parte decimal do tempo está entre 0 e 1.5, 2 e 3.5, 4 e 5.5, 6 e 7.5 ou 8 e 9.5 
alteraImagem2 :: Float -> Bool
alteraImagem2 n = alteraImagem2Aux (mod' n 10)

-- | True quando o número fornecido se situa entre 0 e 1.5, 4 e 5.5, 6 e 7.5 ou 8 e 9.5
alteraImagem2Aux :: Float -> Bool
alteraImagem2Aux n = (n>=0 && n<=1.5) || (n>=2 && n<=3.5) || (n>= 4 && n<=5.5) || (n>=6 && n<= 7.5) || (n>=8 && n<= 9.5)

-- | É chamada automaticamente em intervalos definidos no frame rate, o float recebido representa o tempo decorrido desde a última chamada. Tem diferentes definições para diferentes tipos de menu e é responsável por as consequências das ações dos jogadores
reageTempo :: Float -> Estado -> IO Estado
reageTempo t s | menu s == GanhouJogo                                                                                                                                  = return (s {jogo = Jogo {mapa = mapa (jogo s),inimigos = gravidadeMacaco (realToFrac t) (inimigos (jogo s)), colecionaveis = [], jogador = jogador (jogo s)}, tempo = tempo s + (realToFrac t)})
               | menu s == ModoPausa Continuar || menu s == ModoPausa Reiniciar || menu s == ModoPausa Home || menu s == ModoPausa Controls || menu s == ModoControlos = return (s{jogo = jogo s, tempo = tempo s + (realToFrac t), bonus = bonus s})
               | menu s == ModoJogo                                                                                                                                    = return (ganhaJogo $ perdeJogo $ s {jogo = movimenta (truncate (tempo s)) (tempo s) (jogo s),tempo = tempo s + (realToFrac t), bonus = diminuiBonus (bonus s), saltar = saltar s + (realToFrac t)})
               | menu s == PerdeuJogo                                                                                                                                  = return (s {tempo = tempo s + (realToFrac t), jogo = Jogo{mapa = mapa (jogo s),inimigos = inimigos (jogo s), colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}})
               | menu s == Editor1 || menu s == Editor2                                                                                                                = return (s {jogo = Jogo {mapa = mapa (jogo s), inimigos = verificaInimigo (inimigos (jogo s)) blocos, colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}})
               | otherwise                                                                                                                                             = return (analisaHighScore $ s {jogo = movimenta (truncate (tempo s)) (tempo s) (jogo s),tempo = tempo s + (realToFrac t)})
   where Mapa a b blocos = mapa (jogo s)

-- | Utilizada no editor de mapas, se um inimigo estiver sobreposto com uma plataforma, alçapão ou escada remove-o
verificaInimigo :: [Personagem] -> [[Bloco]] -> [Personagem]
verificaInimigo l blocos = verificaInimigoFant (verificaInimigoMac l blocos) blocos

-- | Auxiliar de verificaInimigo, remove um inimigo se este estiver sobreposto com uma plataforma, alçapão ou escada e for um fantasma
verificaInimigoFant :: [Personagem] -> [[Bloco]] -> [Personagem]
verificaInimigoFant [] _ = []
verificaInimigoFant (h:t) blocos | tipo h == Fantasma = if procuraBloco blocos (posicao h) == Vazio 
                                                          then h : verificaInimigoFant t blocos
                                                          else verificaInimigoFant t blocos
                                 | otherwise = h : verificaInimigoFant t blocos

-- | Auxiliar de verificaInimigo, remove um inimigo se este estiver sobreposto com uma plataforma, alçapão ou escada e for o MacacoMalvado
verificaInimigoMac :: [Personagem] -> [[Bloco]] -> [Personagem]
verificaInimigoMac [] _ = []
verificaInimigoMac (h:t) blocos | tipo h == MacacoMalvado = if procuraBloco blocos (x,y) == Vazio && procuraBloco blocos (x,y+1) == Vazio && procuraBloco blocos (x+1,y) == Vazio && procuraBloco blocos (x+1,y+1) == Vazio && procuraBloco blocos (x-1,y) == Vazio && procuraBloco blocos (x-1,y+1) == Vazio
                                                              then (h:t)
                                                              else h{posicao = (50,50)} : t
                                | otherwise = h : verificaInimigoMac t blocos
  where (x,y) = posicao h

-- | Utilizada para ataulizar o Bonus, faz o bonus diminuir 5 a cada 0.05 segundos, logo diminuir 100 a cada segundo
diminuiBonus :: Int -> Int
diminuiBonus 0 = 0
diminuiBonus n = n - 5

-- | Verifica se o jogador está com 0 vidas, e se sim altera o menu para PerdeuJogo para aparecer a animação de derrota
perdeJogo :: Estado -> Estado
perdeJogo s | vida (jogador (jogo s)) == 0 = s {menu = PerdeuJogo, tempo = 0 ,jogo = Jogo {mapa = mapa (jogo s), inimigos = map ficaParado (inimigos (jogo s)), colecionaveis = colecionaveis (jogo s), jogador = ficaParado(jogador(jogo s)) }}
            | otherwise = s

-- | Altera a velocidade de um personagem para (0,0), fazendo-o ficar parado
ficaParado :: Personagem -> Personagem
ficaParado p = p{velocidade = (0,0)}

-- | Verifica se o jogador está em colisão com a estrela, se sim altera o menu para GanharJOgo para aparecer a animação de vitória
ganhaJogo :: Estado -> Estado
ganhaJogo s | colisaoHitbox (hitboxColecionavel star) (hitbox (jogador (jogo s))) = if editor s == False then s {menu = GanhouJogo, jogo = Jogo {mapa = mapaGanhou,inimigos = ganhouInimigos (inimigos (jogo s)),colecionaveis = [], jogador = jogGanhaJogo (jogador (jogo s)) (bonus s)},tempo=0,highScore = highScore s ++ [(pontos (jogador(jogo s)) + (bonus s),"")]}
                                                                                                         else s {menu = GanhouJogoEditor, jogo = Jogo {mapa = mapa (jogo s),inimigos = ganhouInimigos (inimigos (jogo s)),colecionaveis = [], jogador = jogGanhaJogo (jogador (jogo s)) (bonus s)},tempo = 3,highScore = highScore s}
            | otherwise = s 
   where Mapa a star b = mapa (jogo s)

-- | Reflete os efeitos do jogador ter ganhado o jogo nos inimigos
ganhouInimigos :: [Personagem] -> [Personagem]
ganhouInimigos [] = []
ganhouInimigos (h:t) = h {vida=11} : ganhouInimigos t

-- | Responsavel por atribuir o bonus restante ao jogador quando este ganha o jogo
jogGanhaJogo :: Personagem -> Int -> Personagem
jogGanhaJogo jog b = jog {pontos = (pontos jog) + ((div b 100)*100), velocidade = (0,0)}

-- | Analisa a lista do HighScore e se existirem mais de uma pontuação de mesma pessoa apaga a pontuação menor
analisaHighScore :: Estado -> Estado
analisaHighScore s = s {highScore = analisaHighScoreAux (highScore s)}
  -- where hs = highScore s

-- | Auxiliar de analisaHighScore
analisaHighScoreAux :: [(Int,String)] -> [(Int,String)]
analisaHighScoreAux [] = []
analisaHighScoreAux [a] = [a]
analisaHighScoreAux ((h1,""):(h3,""):t) = (h1,"") : analisaHighScoreAux ((h3,""):t)
analisaHighScoreAux ((h1,"XXXXX"):(h3,"XXXXX"):t) = (h1,"XXXXX") : analisaHighScoreAux ((h3,"XXXXX"):t)
analisaHighScoreAux ((h1,h2):(h3,h4):t) | elem h2 (map snd ((h3,h4):t)) && (h2 == h4) = if h1 > h3 
                                                                                          then (h1,h2) : analisaHighScoreAux t 
                                                                                          else (h3,h4) : analisaHighScoreAux t 
                                        | elem h2 (map snd ((h3,h4):t)) = analisaHighScoreAux ([(h1,h2)] ++ t ++ [(h3,h4)])
                                        | otherwise = (h1,h2) : analisaHighScoreAux ((h3,h4):t)

