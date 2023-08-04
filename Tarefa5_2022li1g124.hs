{- |
Module      : Tarefa5_2022li1g124
Description : Aplicação da desliza Mapa
Copyright   : Frederico Cunha Afonso <a104001@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa5_2022li1g124 where

import LI12223
import Tarefa2_2022li1g124

{-|
== deslizaJogo
A função "__'deslizaJogo'__" é uma função que, recebendo um Int, cria um efeito "deslize" ao 'Mapa',
adicionando 1 à ordenada do 'Jogador', retira o último elemento da lista de tuplos 'Terreno' | lista de 'Obstaculo's e
adiciona um elemento pseudo-aleatório

Podemos assim definir "__'deslizaJogo'__" da seguinte maneira:

=== Definição dada
@
deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo random (Jogo (Jogador (x,y)) (Mapa larg terObs)) = Jogo (Jogador (x,(y+1))) (estendeMapa (Mapa larg (init terObs)) random)
@
-}

deslizaJogo :: Int -> Jogo -> Jogo
deslizaJogo random (Jogo (Jogador (x,y)) (Mapa larg terObs)) = Jogo (Jogador (x,(y+1))) (estendeMapa (Mapa larg (init terObs)) random)