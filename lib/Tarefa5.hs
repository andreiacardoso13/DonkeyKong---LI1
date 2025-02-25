{-|
Module      : Tarefa5
Description : EventKeys
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 5 de LI1 em 2023/24.
-}
module Tarefa5 where

import LI12324
import Imagens
import Mapa
import Music
import Tarefa1
import Tarefa2
import Tarefa3
import Tarefa4
import System.Exit
import Graphics.Gloss
import Graphics.Gloss.Interface.IO.Game

-- | Estado do programa 
data Estado = Estado {menu :: Menu, jogo :: Jogo, imagens :: Imagens, tempo :: Tempo, bonus :: Int, highScore :: [(Int,String)], editor :: Bool, jogoEditor :: Jogo}

-- | Diferentes menus presentes ao longo do jogo
data Menu = Inicio
          | Opcoes Opcao
          | Dificuldade Opcao
          | ModoJogo
          | ModoPausa Opcao
          | ModoHighScore
          | ModoControlos
          | GanhouJogo
          | GanhouJogoEditor
          | PerdeuJogo
          | ModoCreditos
          | Editor1
          | Editor2
          deriving Eq

-- | Opções disponíveis no menu de opções,menu de pausa e nas dificuldades
data Opcao = Jogar
           | HighScore
           | Continuar
           | Reiniciar
           | Home
           | Controls
           | OpCreditos
           | EditorMapas
           | UltJogo
           | Facil 
           | Normal 
           | Dificil
          deriving Eq

-- | Função que recebe e devolve um Estado com as devidas alterações causadas por eventos
keys :: Event -> Estado -> IO Estado
keys evt s | menu s == Inicio              = keysInicio evt s
           | menu s == Opcoes Jogar        = keysOpJogar evt s
           | menu s == Opcoes HighScore    = keysOpHighScore evt s
           | menu s == Opcoes OpCreditos   = keysOpCreditos evt s
           | menu s == Opcoes EditorMapas  = keysOpEditorMapas evt s
           | menu s == Opcoes UltJogo      = keysOpUltJogo evt s
           | menu s == Dificuldade Facil   = keysDificuldadeFacil evt s 
           | menu s == Dificuldade Normal  = keysDificuldadeNormal evt s 
           | menu s == Dificuldade Dificil = keysDificuldadeDificil evt s 
           | menu s == ModoJogo            = keysModoJogo evt s
           | menu s == ModoPausa Continuar = keysModoPausa evt s
           | menu s == ModoPausa Reiniciar = keysModoPausa evt s
           | menu s == ModoPausa Controls  = keysModoPausa evt s
           | menu s == ModoPausa Home      = keysModoPausa evt s
           | menu s == ModoHighScore       = keysModoHighScore evt s
           | menu s == GanhouJogo          = keysGanhouJogo evt s
           | menu s == GanhouJogoEditor    = keysGanhouJogoEditor evt s
           | menu s == PerdeuJogo          = keysPerdeuJogo evt s
           | menu s == ModoControlos       = keysControlos evt s
           | menu s == ModoCreditos        = keysCreditos evt s
           | menu s == Editor1             = keysEditor1 evt s
           | menu s == Editor2             = keysEditor2 evt s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Inicio
keysInicio :: Event -> Estado -> IO Estado
keysInicio (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Opcoes Jogar}
keysInicio (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                            exitSuccess
keysInicio _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Opcoes Jogar
keysOpJogar :: Event -> Estado -> IO Estado
keysOpJogar (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Dificuldade Facil}
keysOpJogar (EventKey (SpecialKey KeyDown) Down _ _) s  = return s {menu = Opcoes OpCreditos}
keysOpJogar (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                             exitSuccess
keysOpJogar _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Opcoes HighScore
keysOpHighScore :: Event -> Estado -> IO Estado
keysOpHighScore (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = ModoHighScore}
keysOpHighScore (EventKey (SpecialKey KeyUp) Down _ _) s    = return s {menu = Opcoes OpCreditos}
keysOpHighScore (EventKey (SpecialKey KeyDown) Down _ _) s  = return s {menu = Opcoes EditorMapas}
keysOpHighScore (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                                 exitSuccess
keysOpHighScore _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Opcoes OpCreditos
keysOpCreditos :: Event -> Estado -> IO Estado
keysOpCreditos (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = ModoCreditos}
keysOpCreditos (EventKey (SpecialKey KeyUp) Down _ _) s    = return s {menu = Opcoes Jogar}
keysOpCreditos (EventKey (SpecialKey KeyDown) Down _ _) s  = return s {menu = Opcoes HighScore}
keysOpCreditos (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                                exitSuccess
keysOpCreditos _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Opcoes EditorMapas
keysOpEditorMapas :: Event -> Estado -> IO Estado
keysOpEditorMapas (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Editor1, jogo = jEditor, editor = True}
keysOpEditorMapas (EventKey (SpecialKey KeyUp) Down _ _) s    = return s {menu = Opcoes HighScore}
keysOpEditorMapas (EventKey (SpecialKey KeyDown) Down _ _) s    = return s {menu = Opcoes UltJogo}
keysOpEditorMapas (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                                   exitSuccess
keysOpEditorMapas _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Opcoes UltJogo
keysOpUltJogo :: Event -> Estado -> IO Estado
keysOpUltJogo (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = ModoJogo, jogo = jogoEditor s,tempo = 0, bonus = 15000, editor = True}
keysOpUltJogo (EventKey (SpecialKey KeyUp) Down _ _) s = return s {menu = Opcoes EditorMapas}
keysOpUltJogo (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                               exitSuccess
keysOpUltJogo _ s = return s



keysDificuldadeFacil :: Event -> Estado -> IO Estado
keysDificuldadeFacil (EventKey (SpecialKey KeyDown) Down _ _) s = return s {menu = Dificuldade Normal}
keysDificuldadeFacil (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = ModoJogo, jogo = j2, tempo = 0, jogoEditor = j2, bonus = 15000, editor = True}
keysDificuldadeFacil (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                                    exitSuccess
keysDificuldadeFacil _ s = return s

keysDificuldadeNormal :: Event -> Estado -> IO Estado
keysDificuldadeNormal (EventKey (SpecialKey KeyDown) Down _ _) s = return s {menu = Dificuldade Dificil}
keysDificuldadeNormal (EventKey (SpecialKey KeyUp) Down _ _) s = return s {menu = Dificuldade Facil}
keysDificuldadeNormal (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = ModoJogo, jogo = j1, tempo = 0, jogoEditor = j1, bonus = 15000}
keysDificuldadeNormal (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                                     exitSuccess
keysDificuldadeNormal _ s = return s

keysDificuldadeDificil :: Event -> Estado -> IO Estado
keysDificuldadeDificil (EventKey (SpecialKey KeyUp) Down _ _) s = return s {menu = Dificuldade Normal}
keysDificuldadeDificil (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = ModoJogo, jogo = j3, tempo = 0, jogoEditor = j3,bonus = 15000, editor = True}
keysDificuldadeDificil (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                                      exitSuccess
keysDificuldadeDificil _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é ModoJogo
keysModoJogo :: Event -> Estado -> IO Estado
keysModoJogo (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                                  inimigos = inimigos,
                                                                                  jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                             emEscada = esc})})}) = return e{jogo = movimentoPers (atualiza (replicate (length inimigos) Nothing) (Just AndarDireita) j {jogador = jgd {posicao = (min x 27.5, y)}})}

keysModoJogo (EventKey (SpecialKey KeyLeft)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                                  inimigos = inimigos,
                                                                                  jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                             emEscada = esc})})}) = return e{jogo = movimentoPers (atualiza (replicate (length inimigos) Nothing) (Just AndarEsquerda) j {jogador = jgd {posicao = (max x 0.5, y)}})}

keysModoJogo (EventKey (SpecialKey KeyUp)    Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                                  inimigos = inimigos,
                                                                                  jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                             emEscada = esc,
                                                                                                             aplicaDano = (b,_)})})}) = return e{jogo = movimentoPers (atualiza (replicate (length inimigos) Nothing) (Just Subir) j)}


keysModoJogo (EventKey (SpecialKey KeyDown)  Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                                  inimigos = inimigos,
                                                                                  jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                             emEscada = esc,
                                                                                                             aplicaDano = (b,_)})})}) = return e{jogo = movimentoPers (atualiza (replicate (length inimigos) Nothing) (Just Descer) j)}

keysModoJogo (EventKey (SpecialKey KeyUp) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                             inimigos = inimigos,
                                                                             jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                   emEscada = esc})})}) | esc && procuraBloco blocos pos == Vazio && procuraBlocoInf blocos pos == Plataforma && colisoesParede m jgd = return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Parar) j {jogador = jgd {emEscada = False}}}
                                                                                                                        | otherwise = return e {jogo = atualiza (replicate (length inimigos) Nothing) (Just Parar) j}

keysModoJogo (EventKey (SpecialKey KeyDown) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                             inimigos = inimigos,
                                                                             jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                   emEscada = esc})})}) | esc  && procuraBlocoInf blocos pos == Plataforma && procuraBloco blocos pos == Escada && colisoesParede m jgd = return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Parar)  j {jogador = jgd {emEscada = False}}}
                                                                                                                        | otherwise = return e {jogo = atualiza (replicate (length inimigos) Nothing) (Just Parar) j}


keysModoJogo (EventKey (SpecialKey k) Up _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                         inimigos = inimigos,
                                                                         jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                           emEscada = esc})})}) = if k == KeyRight || k == KeyLeft
                                                                                                                     then return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Parar) j}
                                                                                                                     else return e


keysModoJogo (EventKey (SpecialKey KeySpace) Down _ _) e@(Estado {jogo = j@(Jogo {mapa = m@(Mapa _ _ blocos),
                                                                                  inimigos = inimigos,
                                                                                  jogador = jgd@(Personagem {posicao = pos@(x,y),
                                                                                                             emEscada = esc,
                                                                                                             aplicaDano = (b,_)})}),
                                                                                                             tempo = t})             = return e{jogo = atualiza (replicate (length inimigos) Nothing) (Just Saltar) j}


keysModoJogo (EventKey (Char 'p') Down _ _) e@(Estado {menu = ModoJogo}) = return e {menu = ModoPausa Continuar}
keysModoJogo (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                            exitSuccess
keysModoJogo _ e = return e

-- | Faz as alterações provocadas por eventos no estado quando o menu é ModoPausa _
keysModoPausa :: Event -> Estado -> IO Estado
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Continuar})                 = return e {menu = ModoJogo}
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Reiniciar})                 = return e {menu = ModoJogo, jogo = jogoEditor e, tempo = 0, bonus = 15000}
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Home})                      = return e {menu = Opcoes Jogar, jogo = jOpcoes}
keysModoPausa (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoPausa Controls})                  = return e {menu = ModoControlos}

keysModoPausa (EventKey (SpecialKey KeyUp) Down _ _)   e@(Estado {menu = ModoPausa Continuar}) = return e {menu = ModoPausa Reiniciar}
keysModoPausa (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {menu = ModoPausa Home})      = return e {menu = ModoPausa Continuar}
keysModoPausa (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {menu = ModoPausa Reiniciar}) = return e {menu = ModoPausa Continuar}
keysModoPausa (EventKey (SpecialKey KeyDown) Down _ _) e@(Estado {menu = ModoPausa Controls})  = return e {menu = ModoPausa Continuar}

keysModoPausa (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {menu = ModoPausa Reiniciar}) = return e {menu = ModoPausa Home}
keysModoPausa (EventKey (SpecialKey KeyRight) Down _ _) e@(Estado {menu = ModoPausa Home})      = return e {menu = ModoPausa Controls}
keysModoPausa (EventKey (SpecialKey KeyLeft) Down _ _)  e@(Estado {menu = ModoPausa Controls})  = return e {menu = ModoPausa Home}
keysModoPausa (EventKey (SpecialKey KeyLeft) Down _ _)  e@(Estado {menu = ModoPausa Home})      = return e {menu = ModoPausa Reiniciar}

keysModoPausa (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                             exitSuccess

keysModoPausa _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é ModoHighScore
keysModoHighScore :: Event -> Estado -> IO Estado
keysModoHighScore (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Opcoes Jogar}
keysModoHighScore (EventKey (SpecialKey KeyEsc) Down _ _) s   = do musicaParar
                                                                   exitSuccess
keysModoHighScore _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é GanhouJogo
keysGanhouJogo :: Event -> Estado -> IO Estado
keysGanhouJogo (EventKey (Char 'a') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "A"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'b') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "B"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'c') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "C"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'd') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "D"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'e') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "E"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'f') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "F"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'g') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "G"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'h') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "H"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'i') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "I"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'j') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "J"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'k') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "K"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'l') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "L"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'm') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "M"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'n') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "N"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'o') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "O"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'p') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "P"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'q') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "Q"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'r') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "R"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 's') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "S"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 't') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "T"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'u') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "U"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'v') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "V"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'w') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "W"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'x') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "X"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'y') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "Y"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (Char 'z') Down _ _) s | length (snd (last (highScore s)))<= 8 = return s {highScore = escreve (highScore s) "Z"}
                                                | otherwise = return s
keysGanhouJogo (EventKey (SpecialKey KeyDelete) Down _ _) s | null (snd (last (highScore s))) = return s
                                                            | otherwise = return s {highScore = remove (highScore s)}
keysGanhouJogo (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Opcoes Jogar, jogo = jOpcoes}
keysGanhouJogo (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                              exitSuccess
keysGanhouJogo _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é GanhouJogoEditor
keysGanhouJogoEditor :: Event -> Estado -> IO Estado
keysGanhouJogoEditor (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Opcoes Jogar, jogo = jOpcoes}
keysGanhouJogoEditor (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                                    exitSuccess
keysGanhouJogoEditor _ s =  return s

-- | Remove o último elemento da String pertencente ao último elemento da lista (remove a última letra escrita)
remove :: [(Int,String)] -> [(Int,String)]
remove [] = []
remove l = init l ++ [(fst (last l), init (snd (last l)))]

-- | Recebe uma lista de tuplos e faz a compactação da String do último tuplo com a String recebida (adiciona a letra selecionada)
escreve :: [(Int,String)] -> String -> [(Int,String)]
escreve l a = init l ++ [(fst (last l), snd (last l) ++ a)]

-- | Faz as alterações provocadas por eventos no estado quando o menu é PerdeuJogo
keysPerdeuJogo :: Event -> Estado -> IO Estado
keysPerdeuJogo (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Opcoes Jogar, jogo = jOpcoes, editor = False}
keysPerdeuJogo (EventKey (Char 'r') Down _ _) e@(Estado {menu = PerdeuJogo, editor = False}) = return e {menu = ModoJogo, jogo = j1, tempo = 0, bonus = 15000}
keysPerdeuJogo (EventKey (Char 'r') Down _ _) e@(Estado {menu = PerdeuJogo, editor = True}) = return e {menu = ModoJogo, jogo = jogoEditor e, tempo = 0, bonus = 15000}
keysPerdeuJogo (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                              exitSuccess
keysPerdeuJogo _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é ModoControlos
keysControlos :: Event -> Estado -> IO Estado
keysControlos (EventKey (SpecialKey KeyEnter) Down _ _) e@(Estado {menu = ModoControlos}) = return e {menu = ModoPausa Controls}
keysControlos (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                             exitSuccess
keysControlos _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é ModoCreditos
keysCreditos :: Event -> Estado -> IO Estado
keysCreditos (EventKey (SpecialKey KeyEnter) Down _ _) s = return s {menu = Opcoes Jogar}
keysCreditos (EventKey (SpecialKey KeyEsc) Down _ _) s = do musicaParar
                                                            exitSuccess
keysCreditos _ s = return s 

-- | Faz as alterações provocadas por eventos no estado quando o menu é Editor1
keysEditor1 :: Event -> Estado -> IO Estado 
keysEditor1 (EventKey (SpecialKey KeyRight) Down _ _) s = if x <27.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x+1,y), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor1 (EventKey (SpecialKey KeyLeft) Down _ _) s = if x >0.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x-1,y), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor1 (EventKey (SpecialKey KeyUp) Down _ _) s = if y >0.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x,y-1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor1 (EventKey (SpecialKey KeyDown) Down _ _) s = if y <16.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x,y+1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor1 (EventKey (SpecialKey KeySpace) Down _ _) s = return s{jogo = Jogo {mapa = atualizaMapa (mapa (jogo s)) (posicao(jogador(jogo s))),inimigos = inimigos (jogo s),colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}}
keysEditor1 (EventKey (Char 'q') Down _ _) s = return s {jogo = Jogo {mapa = mapa (jogo s), inimigos = adicionaFantasma blocos (inimigos (jogo s)) (x,y), colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}}
  where Mapa a b blocos = mapa (jogo s)
        (x,y) = posicao (jogador (jogo s))
keysEditor1 (EventKey (Char 'w') Down _ _) s = return s {jogo = Jogo {mapa = mapa (jogo s), inimigos = adicionaMacaco blocos (inimigos (jogo s)) (x,y), colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}}
 where Mapa a b blocos = mapa (jogo s)
       (x,y) = posicao (jogador (jogo s))
keysEditor1 (EventKey (Char 'e') Down _ _) s = return s {jogo = Jogo {mapa = mapa (jogo s), inimigos = inimigos (jogo s), colecionaveis = colecionaveis (jogo s) ++ [(Moeda,posicao (jogador (jogo s)))], jogador = jogador (jogo s)}}
keysEditor1 (EventKey (Char 'r') Down _ _) s = return s {jogo = Jogo {mapa = mapa (jogo s), inimigos = inimigos (jogo s), colecionaveis = colecionaveis (jogo s) ++ [(Martelo,posicao (jogador (jogo s)))], jogador = jogador (jogo s)}}
keysEditor1 (EventKey (Char 't') Down _ _) s = return s {jogo = Jogo {mapa = Mapa a (posicao (jogador (jogo s))) b, inimigos = inimigos (jogo s), colecionaveis = colecionaveis (jogo s), jogador = jogador (jogo s)}}
  where Mapa a star b = mapa (jogo s)
keysEditor1 (EventKey (SpecialKey KeyDelete) Down _ _) s = return s {jogo = Jogo {mapa = mapa (jogo s), inimigos = [],colecionaveis = [], jogador = jogador (jogo s)}}
keysEditor1 (EventKey (SpecialKey KeyEnter) Down _ _) s = if valida (jogo s) 
                                                             then return s {menu=Editor2}
                                                             else return s
keysEditor1 (EventKey (SpecialKey KeyEsc) Down _ _) s = return s {menu = Opcoes EditorMapas}
keysEditor1 _ s = return s

-- | Faz as alterações provocadas por eventos no estado quando o menu é Editor2
keysEditor2 :: Event -> Estado -> IO Estado
keysEditor2 (EventKey (SpecialKey KeyRight) Down _ _) s = if x <27.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x+1,y), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor2 (EventKey (SpecialKey KeyLeft) Down _ _) s = if x >0.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x-1,y), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor2 (EventKey (SpecialKey KeyUp) Down _ _) s = if y >0.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x,y-1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor2 (EventKey (SpecialKey KeyDown) Down _ _) s = if y <16.5 then return s {jogo = Jogo {mapa= (mapa (jogo s)), inimigos = (inimigos (jogo s)), colecionaveis = (colecionaveis (jogo s)), jogador = Personagem {velocidade = (0,0), tipo = Jogador, posicao =(x,y+1), direcao = Este, tamanho = (1,1), emEscada = False, ressalta = False, vida = 3, pontos = 0, aplicaDano = (False,0)}}}
                                                                    else return s
    where (x,y) = posicao (jogador (jogo s))
keysEditor2 (EventKey (SpecialKey KeyEnter) Down _ _) s = if valida (jogo s) 
                                                              then return s {menu = ModoJogo, jogo = atualizaPosicaoInicial (jogo s), jogoEditor = jogo s, bonus = 15000}
                                                              else return s
keysEditor2 (EventKey (SpecialKey KeyEsc) Down _ _) s = return s {menu = Editor1}
keysEditor2 _ s = return s

-- | Atualiza a posição inicial no mapa tendo em conta a posição atual do jogador (utilizada quando o jogo começa no editor de mapas)
atualizaPosicaoInicial :: Jogo -> Jogo
atualizaPosicaoInicial j = j {mapa = Mapa (posicao (jogador j), direcao (jogador j)) b c}
   where Mapa a b c = mapa j

-- | Adiciona o Macaco Malvado à lista de inimigos (caso este não pertença) ou altera a sua posição para a fornecida (caso este já pertença), mas apenas no caso deste se localizar em blocos do tipo Vazio
adicionaMacaco :: [[Bloco]] -> [Personagem] -> Posicao -> [Personagem]
adicionaMacaco blocos [] (x,y) = [Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (x,y-0.5), direcao = Este, tamanho = (2.49,2), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
adicionaMacaco blocos (h:t) (x,y) | verificaMacaco (h:t) == False = (h:t) ++ [Personagem {velocidade = (0,0), tipo = MacacoMalvado, posicao = (x,y-0.5), direcao = Este, tamanho = (2.49,2), emEscada = False, ressalta = True, vida = 1, pontos = 0, aplicaDano = (False,0)}]
                                  | otherwise = if tipo h == MacacoMalvado 
                                                  then h {posicao = (x,y-0.5)} :t 
                                                  else h : adicionaMacaco blocos t (x,y)

-- | Verifica se o Macaco Malvado consta da lista de inimigos
verificaMacaco :: [Personagem] -> Bool
verificaMacaco [] = False
verificaMacaco (h:t) | tipo h == MacacoMalvado = True
                     | otherwise = verificaMacaco t

-- | Adiciona um Fantasma à lista de inimigos, mas apenas no caso deste se localizar em blocos do tipo Vazio
adicionaFantasma :: [[Bloco]] -> [Personagem] -> Posicao -> [Personagem]
adicionaFantasma blocos l (x,y) | procuraBloco blocos (x,y) == Vazio && procuraBloco blocos (x,y-1) == Vazio = l ++ [Personagem {velocidade = (0,0), tipo = Fantasma, posicao = (x,y), direcao = Este, tamanho=(1,1),emEscada = False, ressalta = True,vida = 1,pontos = 0, aplicaDano = (False,0)}]
                                | otherwise = l

-- | Devolve um mapa atualizado pela mudança do bloco onde se localiza a posição fornecida
atualizaMapa :: Mapa -> Posicao -> Mapa
atualizaMapa (Mapa a b blocos) pos = Mapa a b (alteraBloco blocos pos)

-- | Devolve uma matriz de blocos atualizada pela mudança do bloco onde se localiza a posição fornecida
alteraBloco :: [[Bloco]] -> Posicao -> [[Bloco]]
alteraBloco (h:t) (x,y) | y == 0.5 = alteraBloco2 h (x,y) : t
                        | otherwise = h : alteraBloco t (x,y-1)

-- | Devolve uma lista de blocos atualizada pela mudança do bloco onde se localiza a posição fornecida
alteraBloco2 :: [Bloco] -> Posicao -> [Bloco]
alteraBloco2 (h:t) (x,y) | x == 0.5 = alteraBloco3 h : t 
                         | otherwise = h : alteraBloco2 t (x-1,y)

-- | Altera um bloco
alteraBloco3 :: Bloco -> Bloco
alteraBloco3 b | b == Plataforma = Alcapao
               | b == Alcapao = Escada
               | b == Escada = Vazio
               | otherwise = Plataforma