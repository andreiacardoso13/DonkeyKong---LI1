{-|
Module      : Tarefa2
Description : Valida jogo
Copyright   : Andreia Alves Cardoso <a106915@alunos.uminho.pt>
              Cátia Alexandra Ribeiro da Eira <a107382@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 de LI1 em 2023/24.
-}
module Tarefa2 where

import LI12324

valida :: Jogo -> Bool
valida = undefined

valida1 :: Jogo -> Bool
valida1 (Jogo {mapa = Mapa _ _ (h:t)}) = valida1Aux (last t)

valida1Aux :: [Bloco] -> Bool
valida1Aux [] = True
valida1Aux (h:t) | h == Plataforma = valida1Aux t
                 | otherwise = False

valida4 :: Jogo -> Bool -- ainda não testada
valida4 (Jogo {inimigos = l }) = length l >= 2 

{-
data Mapa =
  Mapa (Posicao, Direcao) Posicao [[Bloco]]
  deriving (Eq, Read, Show)


data Jogo =
  Jogo
    { mapa          :: Mapa -- ^ mapa do jogo
    , inimigos      :: [Personagem] -- ^ lista de inimigos no mapa
    , colecionaveis :: [(Colecionavel, Posicao)] -- ^ lista de colecionaveis espalhados pelo mapa
    , jogador       :: Personagem -- ^ o jogador
    }
  deriving (Eq, Read, Show)
  
  hitbox :: Personagem -> Hitbox
hitbox (Personagem {posicao = (x,y), tamanho 

data Personagem =
  Personagem
    { velocidade :: Velocidade
    , tipo       :: Entidade
    , posicao    :: Posicao
    , direcao    :: Direcao
    , tamanho    :: (Double, Double)
    , emEscada   :: Bool -- ^ se está numa escada
    , ressalta   :: Bool
    , vida       :: Int -- ^ não negativo
    , pontos     :: Int
    , aplicaDano :: (Bool, Double) -- ^ se está armado e por quanto tempo ainda
    }
  deriving (Eq, Read, Show)
  -}
