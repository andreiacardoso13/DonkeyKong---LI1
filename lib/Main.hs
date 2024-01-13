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
import Desenha


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
estadoInicial images = Estado {menu = Inicio,jogo = jOpcoes, imagens = images, tempo = 0 ,bonus = 15000, highScore = [(0,"XXXXX"), (0,"XXXXX"),(0,"XXXXX")], saltar = 2, editor = False, jogoEditor = j1}

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
perdeJogo s | vida (jogador (jogo s)) == 0 = s {menu = PerdeuJogo, tempo = 0 ,jogo = Jogo {mapa = mapa (jogo s), inimigos = map ficaParado (inimigos (jogo s)), colecionaveis = colecionaveis (jogo s), jogador = ficaParado(jogador(jogo s)) }, editor = False}
            | otherwise = s

-- | Altera a velocidade de um personagem para (0,0), fazendo-o ficar parado
ficaParado :: Personagem -> Personagem
ficaParado p = p{velocidade = (0,0)}

-- | Verifica se o jogador está em colisão com a estrela, se sim altera o menu para GanharJOgo para aparecer a animação de vitória
ganhaJogo :: Estado -> Estado
ganhaJogo s | colisaoHitbox (hitboxColecionavel star) (hitbox (jogador (jogo s))) = if editor s == False then s {menu = GanhouJogo, jogo = Jogo {mapa = mapaGanhou,inimigos = ganhouInimigos (inimigos (jogo s)),colecionaveis = [], jogador = jogGanhaJogo (jogador (jogo s)) (bonus s)},tempo=0,highScore = highScore s ++ [(pontos (jogador(jogo s)) + (bonus s),"")]}
                                                                                                         else s {menu = GanhouJogoEditor, jogo = Jogo {mapa = mapa (jogo s),inimigos = ganhouInimigos (inimigos (jogo s)),colecionaveis = [], jogador = jogGanhaJogo (jogador (jogo s)) (bonus s)},tempo = 3,highScore = highScore s, editor = False}
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

