module Main where

import LI12223
import Tarefa1_2022li1g124
import Tarefa2_2022li1g124
import Tarefa3_2022li1g124
import Tarefa4_2022li1g124
import Tarefa5_2022li1g124
import Tarefa6_2022li1g124
import System.Random
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

main :: IO ()
main = do 
        trueRandom <- randomRIO (1 :: Int, 1000000000 :: Int)
        upKey <- loadBMP "Instrucoes/UpKey.bmp"
        downKey <- loadBMP "Instrucoes/DownKey.bmp"
        leftKey <- loadBMP "Instrucoes/LeftKey.bmp"
        rightKey <- loadBMP "Instrucoes/RightKey.bmp"
        space <- loadBMP "Instrucoes/Space.bmp"
        enter <- loadBMP "Instrucoes/Enter.bmp"
        q <- loadBMP "Instrucoes/Q.bmp"
        r <- loadBMP "Instrucoes/R.bmp"
        frogger_b1 <- loadBMP "Frogger/bombeirosBack.bmp"
        frogger_b2 <- loadBMP "Frogger/bombeirosMID.bmp"
        frogger_b3 <- loadBMP "Frogger/BombeirosFrente.bmp"
        frogger_c <- loadBMP "Frogger/car1.bmp"
        frogger_t1 <- loadBMP "Frogger/truckSTART.bmp"
        frogger_t2 <- loadBMP "Frogger/truckEnd.bmp"
        frogger_l1 <- loadBMP "Frogger/logSTART.bmp"
        frogger_l2 <- loadBMP "Frogger/logMID.bmp"
        frogger_l3 <- loadBMP "Frogger/logEnd.bmp"
        truefrogger <- loadBMP "Frogger/frogger.bmp"
        frogger_t <- loadBMP "Frogger/tree.bmp"
        frogger_re <- loadBMP "Frogger/relva.bmp"
        cluckcluck <- loadBMP "Objects/cluckcluck.bmp"
        tronco <- loadBMP "Objects/log.bmp"
        startTronco <- loadBMP "Objects/StartTronco.bmp"
        arvore <- loadBMP "Objects/tree.bmp"
        frogger <- loadBMP "Objects/frogger.bmp"
        relva <- loadBMP "Classic/relva.bmp"
        rio <- loadBMP "Classic/river.bmp"
        estrada <- loadBMP "Classic/road.bmp"
        road8 <- loadBMP "3D/road8.bmp"
        road7 <- loadBMP "3D/road7.bmp"
        road6 <- loadBMP "3D/road6.bmp"
        road5 <- loadBMP "3D/road5.bmp"
        road4 <- loadBMP "3D/road4.bmp"
        road3 <- loadBMP "3D/road3.bmp"
        road2 <- loadBMP "3D/road2.bmp"
        road1 <- loadBMP "3D/road1.bmp"
        road0 <- loadBMP "3D/road0.bmp"
        river8 <- loadBMP "3D/river8.bmp"
        river7 <- loadBMP "3D/river7.bmp"
        river6 <- loadBMP "3D/river6.bmp"
        river5 <- loadBMP "3D/river5.bmp"
        river4 <- loadBMP "3D/river4.bmp"
        river3 <- loadBMP "3D/river3.bmp"
        river2 <- loadBMP "3D/river2.bmp"
        river1 <- loadBMP "3D/river1.bmp"
        river0 <- loadBMP "3D/river0.bmp"
        relva8 <- loadBMP "3D/relva8.bmp"
        relva7 <- loadBMP "3D/relva7.bmp"
        relva6 <- loadBMP "3D/relva6.bmp"
        relva5 <- loadBMP "3D/relva5.bmp"
        relva4 <- loadBMP "3D/relva4.bmp"
        relva3 <- loadBMP "3D/relva3.bmp"
        relva2 <- loadBMP "3D/relva2.bmp"
        relva1 <- loadBMP "3D/relva1.bmp"
        relva0 <- loadBMP "3D/relva0.bmp"
        let glossy = [[arvore,tronco,cluckcluck,frogger,(scale (-1) 1 frogger),(scale 0.8 0.8 cluckcluck),startTronco],
                      [relva,rio,estrada],
                      [road8,road7,road6,road5,road4,road3,road2,road1,road0],
                      [river8,river7,river6,river5,river4,river3,river2,river1,river0],
                      [relva8,relva7,relva6,relva5,relva4,relva3,relva2,relva1,relva0],
                      (map (scale 0.75 0.75) [(rotate 90 frogger_b1),(rotate 90 frogger_b2),(rotate 90 frogger_b3),(rotate 180 frogger_c),(rotate 180 frogger_t1),(rotate 180 frogger_t2),frogger_l1,frogger_l2,frogger_l3,truefrogger,frogger_t,frogger_re]),
                      [upKey,downKey,leftKey,rightKey,space,enter,q,r]]
        play  dm             -- janela onde irá decorrer o jogo
              black    -- cor do fundo da janela
              fr             -- frame rate
              (worldInicial glossy (jogoInicial 0 Undefined) trueRandom)  -- define estado inicial do jogo
              worldBuilder   -- desenha o estado do jogo
              worldReaction    -- reage a um evento
              tokioTomare     -- reage ao passar do tempo