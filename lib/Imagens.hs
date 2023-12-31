module Imagens where
import Graphics.Gloss
import Graphics.Gloss.Juicy

data Imagem = Ladder | Trapdoor | Platform | Estrela | Coin | Hammer | GhostLeft1 | GhostLeft2 | GhostRight1 | GhostRight2 | MarioClimbing1 | MarioClimbing2 | MarioHammerLeft1 | MarioHammerLeft2 | MarioHammerLeft3 | MarioHammerLeft4 | MarioHammerRight1 | MarioHammerRight2 | MarioHammerRight3 | MarioHammerRight4 | MarioJumpingLeft1 | MarioJumpingRight1 | MarioStandingBack | MarioStandingLeft | MarioStandingRight | MarioWalkingLeft1 | MarioWalkingRight1 | MonkeyDefeated | MonkeyFalling | MonkeyStanding | VidaCheia | VidaVazia | ZeroVidas | UmaVida | DuasVidas | TresVidas | Num0 | Num1 | Num2 | Num3 | Num4 | Num5 | Num6 | Num7 | Num8 | Num9 | Score | Bonus | MarioDefeated1 | MarioDefeated2 | MarioDefeated3 | MarioDefeated4 | MarioDefeatedFinal | GhostDefeated1 | GhostDefeated2 | GhostDefeated3 | GhostDefeated4 | Congratulations deriving (Eq)

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
               Just monkeyStanding      <- loadJuicyPNG "imagens/monkeyStanding.png"
               Just vidaCheia           <- loadJuicyPNG "imagens/vidacheia.png"
               Just vidaVazia           <- loadJuicyPNG "imagens/vidavazia.png"
               Just zeroVidas           <- loadJuicyPNG "imagens/vida0.png"
               Just umaVida             <- loadJuicyPNG "imagens/vida1.png"
               Just duasVidas           <- loadJuicyPNG "imagens/vida2.png"
               Just tresVidas           <- loadJuicyPNG "imagens/vida3.png"
               Just num0                <- loadJuicyPNG "imagens/num0.png"
               Just num1                <- loadJuicyPNG "imagens/num1.png"
               Just num2                <- loadJuicyPNG "imagens/num2.png"
               Just num3                <- loadJuicyPNG "imagens/num3.png"
               Just num4                <- loadJuicyPNG "imagens/num4.png"
               Just num5                <- loadJuicyPNG "imagens/num5.png"
               Just num6                <- loadJuicyPNG "imagens/num6.png"
               Just num7                <- loadJuicyPNG "imagens/num7.png"
               Just num8                <- loadJuicyPNG "imagens/num8.png"
               Just num9                <- loadJuicyPNG "imagens/num9.png"
               Just score               <- loadJuicyPNG "imagens/score.png"
               Just bonus               <- loadJuicyPNG "imagens/bonus.png"
               Just marioDefeated1      <- loadJuicyPNG "imagens/marioDefeated1.png" 
               Just marioDefeated2      <- loadJuicyPNG "imagens/marioDefeated2.png"
               Just marioDefeated3      <- loadJuicyPNG "imagens/marioDefeated3.png"
               Just marioDefeated4      <- loadJuicyPNG "imagens/marioDefeated4.png"
               Just marioDefeatedFinal  <- loadJuicyPNG "imagens/marioDefeatedFinal.png"
               Just ghostDefeated1      <- loadJuicyPNG "imagens/ghostDefeated1.png"
               Just ghostDefeated2      <- loadJuicyPNG "imagens/ghostDefeated2.png"
               Just ghostDefeated3      <- loadJuicyPNG "imagens/ghostDefeated3.png"
               Just ghostDefeated4      <- loadJuicyPNG "imagens/ghostDefeated4.png"
               Just congratulations     <- loadJuicyPNG "imagens/congratulations.png"

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
                             (MonkeyDefeated,monkeyDefeated), (MonkeyFalling,monkeyFalling), (MonkeyStanding,monkeyStanding),
                             (VidaCheia,vidaCheia), (VidaVazia,vidaVazia), (ZeroVidas,zeroVidas), (UmaVida,umaVida),
                             (DuasVidas,duasVidas), (TresVidas,tresVidas), (Num0,num0), (Num1,num1), (Num2,num2),
                             (Num3,num3), (Num4,num4), (Num5,num5), (Num6,num6), (Num7,num7), (Num8,num8), (Num9,num9),
                             (Score,score), (Bonus,bonus), (MarioDefeated1,marioDefeated1), (MarioDefeated2,marioDefeated2),
                             (MarioDefeated3,marioDefeated3), (MarioDefeated4,marioDefeated4), (MarioDefeatedFinal,marioDefeatedFinal),
                             (GhostDefeated1,ghostDefeated1), (GhostDefeated2,ghostDefeated2), (GhostDefeated3,ghostDefeated3),
                             (GhostDefeated4,ghostDefeated4), (Congratulations,congratulations)]
               return images