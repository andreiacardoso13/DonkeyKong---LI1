module Imagens where
import Graphics.Gloss
import Graphics.Gloss.Juicy

data Imagens = Imagens {
    escada :: Picture,
    alcapao :: Picture,
    plataforma :: Picture,
    estrela :: Picture,
    moeda :: Picture,
    vazio :: Picture
    }

getImages :: IO [Picture]
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

               let images = [escada, alcapao, plataforma, estrela, moeda, martelo, ghostLeft1,
                             ghostLeft2, ghostRight1, ghostRight2, marioClimbing1, marioClimbing2,
                             marioHammerLeft1, marioHammerLeft2, marioHammerLeft3, marioHammerLeft4,
                             marioHammerRight1, marioHammerRight2, marioHammerRight3, marioHammerRight4,
                             marioJumpingLeft1, marioJumpingRight1, marioStandingBack, marioStandingLeft,
                             marioStandingRight, marioWalkingLeft1, marioWalkingRight1, monkeyDefeated, monkeyFalling]
               return images