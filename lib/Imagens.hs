module Imagens where
import Graphics.Gloss
import Graphics.Gloss.Juicy

data Imagem = Ladder | Trapdoor | Platform | Estrela | Coin | Hammer | GhostLeft1 | GhostLeft2 | GhostRight1 | GhostRight2 | MarioClimbing1 | MarioClimbing2 | MarioHammerLeft1 | MarioHammerLeft2 | MarioHammerLeft3 | MarioHammerLeft4 | MarioHammerRight1 | MarioHammerRight2 | MarioHammerRight3 | MarioHammerRight4 | MarioJumpingLeft1 | MarioJumpingRight1 | MarioStandingBack | MarioStandingLeft | MarioStandingRight | MarioWalkingLeft1 | MarioWalkingRight1 | MonkeyDefeated | MonkeyFalling deriving (Eq)

type Imagens = [(Imagem, Picture)]

getImages :: IO Imagens
getImages = do
               Just escada              <- loadJuicyPNG "imagens/ladder.png"
               Just alcapao             <- loadJuicyPNG "imagens/alcapao.png"
               Just plataforma          <- loadJuicyPNG "imagens/plataforma.png"
               Just estrela             <- loadJuicyPNG "imagens/estrela.png"
               Just moeda               <- loadJuicyPNG "imagens/moeda.png"
               Just martelo             <- loadJuicyPNG "imagens/hammer.png"
--             Just vazio               <- loadJuicyPNG "imagens/vazio.png"
               Just ghostLeft1          <- loadJuicyPNG "imagens/ghostLeft1.png"
               Just ghostLeft2          <- loadJuicyPNG "imagens/ghostLeft2.png"
               Just ghostRight1         <- loadJuicyPNG "imagens/ghostRight1.png"
               Just ghostRight2         <- loadJuicyPNG "imagens/ghostRight2.png"
               Just marioClimbing1      <- loadJuicyPNG "imagens/marioClimbing1.png"
               Just marioClimbing2      <- loadJuicyPNG "imagens/marioClimbing2.png"
               Just marioHammerLeft1    <- loadJuicyPNG "imagens/marioHammerLeft1.png"
               Just marioHammerLeft2    <- loadJuicyPNG "imagens/marioHammerLeft2.png"
               Just marioHammerLeft3    <- loadJuicyPNG "imagens/marioHammerLeft3.png"
               Just marioHammerLeft4    <- loadJuicyPNG "imagens/marioHammerLeft4.png"
               Just marioHammerRight1   <- loadJuicyPNG "imagens/marioHammerRight1.png"
               Just marioHammerRight2   <- loadJuicyPNG "imagens/marioHammerRight2.png"
               Just marioHammerRight3   <- loadJuicyPNG "imagens/marioHammerRight3.png"
               Just marioHammerRight4   <- loadJuicyPNG "imagens/marioHammerRight4.png"
               Just marioJumpingLeft1   <- loadJuicyPNG "imagens/marioJumpingLeft1.png"
               Just marioJumpingRight1  <- loadJuicyPNG "imagens/marioJumpingRight1.png"
               Just marioStandingBack   <- loadJuicyPNG "imagens/marioStandingBack.png"
               Just marioStandingLeft   <- loadJuicyPNG "imagens/marioStandingLeft.png"
               Just marioStandingRight  <- loadJuicyPNG "imagens/marioStandingRight.png"
               Just marioWalkingLeft1   <- loadJuicyPNG "imagens/marioWalkingLeft1.png"
               Just marioWalkingRight1  <- loadJuicyPNG "imagens/marioWalkingRight1.png"
               Just monkeyDefeated      <- loadJuicyPNG "imagens/monkeyDefeated.png"
               Just monkeyFalling       <- loadJuicyPNG "imagens/monkeyFalling.png"

               let images = [(Ladder,escada), (Trapdoor,alcapao), (Platform,plataforma), (Estrela,estrela),
                             (Coin,moeda), (Hammer,martelo), (GhostLeft1,ghostLeft1), (GhostLeft2,ghostLeft2),
                             (GhostRight1,ghostRight1), (GhostRight2,ghostRight2), (MarioClimbing1,marioClimbing1),
                             (MarioClimbing2,marioClimbing2), (MarioHammerLeft1,marioHammerLeft1),
                             (MarioHammerLeft2,marioHammerLeft2), (MarioHammerLeft3,marioHammerLeft3),
                             (MarioHammerLeft4,marioHammerLeft4), (MarioHammerRight1,marioHammerRight1),
                             (MarioHammerRight2,marioHammerRight2), (MarioHammerRight3,marioHammerRight3),
                             (MarioHammerRight4,marioHammerRight4), (MarioJumpingLeft1,marioJumpingLeft1),
                             (MarioJumpingRight1,marioJumpingRight1), (MarioStandingBack,marioStandingBack),
                             (MarioStandingLeft,marioStandingLeft), (MarioStandingRight,marioStandingRight),
                             (MarioWalkingLeft1,marioWalkingLeft1), (MarioWalkingRight1,marioWalkingRight1),
                             (MonkeyDefeated,monkeyDefeated), (MonkeyFalling,monkeyFalling)]
               return images