{- |
Module      : Tarefa3_2022li1g124
Description : Movimentação do personagem e obstáculos
Copyright   : Frederico Cunha Afonso <a104001@alunos.uminho.pt>

Módulo para a realização da Tarefa 3 do projeto de LI1 em 2022/23.
-}
module Tarefa3_2022li1g124 where

import LI12223
import Tarefa2_2022li1g124


{-|
== animaJogo
A função "__'animaJogo'__" é uma função que, dado um 'Jogo' e uma 'Jogada'
permite alterar as 'Coordenadas' do 'Jogador' e os 'Obstaculo's presentes 
no 'Mapa'

=== Definição dada
@
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa larg terObs)) (Parado)
    | (snd(terObs !! y)) !! x == Tronco = Jogo (Jogador (x + vel,y)) (Mapa larg (animaobs terObs 0 (x,y)))
    | otherwise                         = Jogo (Jogador (x,y)) (Mapa larg (animaobs terObs 0 (x,y)))
        where vel = velTer1(fst (terObs !! y))
animaJogo (Jogo (Jogador (x,y)) (Mapa larg terObs)) (Move dir)
    = (Jogo (Jogador (corrente (limites terObs dir (x,y) larg) terObs)) (Mapa larg (animaobs terObs 0 (limites terObs dir (x,y) larg))))
@
-}
animaJogo :: Jogo -> Jogada -> Jogo
animaJogo (Jogo (Jogador (x,y)) (Mapa larg terObs)) (Parado)
    | (snd(terObs !! y)) !! x == Tronco = Jogo (Jogador (x + vel,y)) (Mapa larg (animaobs terObs 0 (x,y)))
    | otherwise                         = Jogo (Jogador (x,y)) (Mapa larg (animaobs terObs 0 (x,y)))
        where vel = velTer1(fst (terObs !! y))
animaJogo (Jogo (Jogador (x,y)) (Mapa larg terObs)) (Move dir)
    = (Jogo (Jogador (corrente (limites terObs dir (x,y) larg) terObs)) (Mapa larg (animaobs terObs 0 (limites terObs dir (x,y) larg))))

----------------------------------------------------------------------------------------------------------------------------
{-|
== animaobs
A função "__'animaobs'__" é uma função que, desloca os 'Obstaculo's numa certa 
direção dependendo da 'Velocidade' do 'Terreno'

=== Definição dada
@
animaobs :: [(Terreno, [Obstaculo])] -> Int -> Coordenadas -> [(Terreno, [Obstaculo])]
animaobs [] _ _= []
animaobs ((Relva, obs):t) _ (x,y) = ((Relva, obs):(animaobs t 0 (x,(y-1))))
animaobs (((Estrada vel),obs):t) k (x,y) 
    | k == vel || (((y == 0) && (obs !! x == Carro))) = ((Estrada vel), obs):(animaobs t 0 (x,(y-1)))
    | vel < 0                                         = animaobs (((Estrada vel),((tail obs) ++ [head obs])):t) (k-1) (x,y)
    | vel > 0                                         = animaobs (((Estrada vel),((last obs):(init obs))):t) (k+1) (x,y)
animaobs (((Rio vel),obs):t) k (x,y) 
    | k == vel = ((Rio vel), obs):(animaobs t 0 (x,(y-1)))
    | vel < 0  = animaobs (((Rio vel),((tail obs) ++ [head obs])):t) (k-1) (x,y)
    | vel > 0  = animaobs (((Rio vel),((last obs):(init obs))):t) (k+1) (x,y)
@
-}
animaobs :: [(Terreno, [Obstaculo])] -> Int -> Coordenadas -> [(Terreno, [Obstaculo])]
animaobs [] _ _= []
animaobs ((Relva, obs):t) _ (x,y) = ((Relva, obs):(animaobs t 0 (x,(y-1))))
animaobs (((Estrada vel),obs):t) k (x,y) 
    | k == vel || (((y == 0) && (obs !! x == Carro))) = ((Estrada vel), obs):(animaobs t 0 (x,(y-1)))
    | vel < 0                                         = animaobs (((Estrada vel),((tail obs) ++ [head obs])):t) (k-1) (x,y)
    | vel > 0                                         = animaobs (((Estrada vel),((last obs):(init obs))):t) (k+1) (x,y)
animaobs (((Rio vel),obs):t) k (x,y) 
    | k == vel = ((Rio vel), obs):(animaobs t 0 (x,(y-1)))
    | vel < 0  = animaobs (((Rio vel),((tail obs) ++ [head obs])):t) (k-1) (x,y)
    | vel > 0  = animaobs (((Rio vel),((last obs):(init obs))):t) (k+1) (x,y)

----------------------------------------------------------------------------------------------------------------------------              
{-|
== corrente
A função "__'corrente'__" é uma função que, verifica se o 'Jogador' 
se encontra num 'Tronco ou não

=== Definição dada
@
corrente :: Coordenadas -> [(Terreno, [Obstaculo])] -> Coordenadas
corrente (x,y) terobs | (snd (terobs !! y))!!x == Tronco = (x+(velTer1 (fst(terobs !! y))),y)
                      | otherwise                        = (x,y)  
@
-}
corrente :: Coordenadas -> [(Terreno, [Obstaculo])] -> Coordenadas
corrente (x,y) terobs | (snd (terobs !! y))!!x == Tronco = (x+(velTer1 (fst(terobs !! y))),y)
                      | otherwise                        = (x,y)
                      
{-|
== limites
A função "__'limites'__" é uma função que, recebendo uma 
lista de tuplos 'Terreno's | lista de 'Obstaculo's, 
'Coordenadas' e 'Largura', devolve novas 'Coordenadas', 
vendo se o 'Jogador' pode ou não efetuar uma dada uma 'Direcao' 
Podemos assim definir "__'limites'__" da seguinte maneira:

=== Definição dada
@
limites :: [(Terreno, [Obstaculo])] -> Direcao -> Coordenadas -> Largura -> Coordenadas
limites terObs Cima (x, y) larg
    | y == 0 || snd (terObs !! (y-1)) !! x == Arvore = (x,y) 
    | otherwise                                      = (x,y-1)
limites terObs Baixo (x, y) larg
    | (y +1) == length terObs || snd (terObs !! (y+1)) !! x == Arvore = (x,y) 
    | otherwise                                                       = (x,y+1)
limites terObs Esquerda (x, y) larg
    | x == 0 || snd (terObs !! y) !! (x-1) == Arvore = (x,y)
    | otherwise                                      = (x-1,y)
        where vel = velTer1(fst (terObs !! y))
limites terObs Direita (x, y) larg
    | (x+1) == larg || snd (terObs !! y) !! (x+1) == Arvore = (x,y)
    | otherwise                                             = (x+1,y)
        where vel = velTer1(fst (terObs !! y))   
@
-}
limites :: [(Terreno, [Obstaculo])] -> Direcao -> Coordenadas -> Largura -> Coordenadas
limites terObs Cima (x, y) larg
    | y == 0 || snd (terObs !! (y-1)) !! x == Arvore = (x,y) 
    | otherwise                                      = (x,y-1)
limites terObs Baixo (x, y) larg
    | (y +1) == length terObs || snd (terObs !! (y+1)) !! x == Arvore = (x,y) 
    | otherwise                                                       = (x,y+1)
limites terObs Esquerda (x, y) larg
    | x == 0 || snd (terObs !! y) !! (x-1) == Arvore = (x,y)
    | otherwise                                      = (x-1,y)
        where vel = velTer1(fst (terObs !! y))
limites terObs Direita (x, y) larg
    | (x+1) == larg || snd (terObs !! y) !! (x+1) == Arvore = (x,y)
    | otherwise                                             = (x+1,y)
        where vel = velTer1(fst (terObs !! y))
----------------------------------------------------------------------------------------------------------------------------
