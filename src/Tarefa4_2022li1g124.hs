{- |
Module      : Tarefa4_2022li1g124
Description : Determinar se o jogo terminou
Copyright   : Frederico Cunha Afonso <a104001@alunos.uminho.pt>

Módulo para a realização da Tarefa 4 do projeto de LI1 em 2022/23.
-}
module Tarefa4_2022li1g124 where

import LI12223
{-|
== jogoTerminou
A função "__'jogoTerminou'__" é uma função que, auxiliada pelas "__'atropelado'__", "__'foradelimites'__", "__'afogou'__",
consegue receber um '__Jogo__' e determinar se o Jogador ultrapassou um limite, afundou-se no rio ou se foi atropelado. Isto
dado um "__Mapa__" e devolvendo um Booleano.

Podemos assim definir "__'jogoTerminou'__" da seguinte maneira:

=== Definição dada
@
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa larg ((ter , obs):t))) = (foradelimites larg ((ter, obs):t) (x,y)) || (atropelado ((ter, obs):t) (x,y)) ||  (afogou ((ter, obs):t) (x,y))
@
-}
----------------------------------------------------------------------------------------------------
jogoTerminou :: Jogo -> Bool
jogoTerminou (Jogo (Jogador (x,y)) (Mapa larg ((ter , obs):t))) = (foradelimites larg ((ter, obs):t) (x,y)) || (atropelado ((ter, obs):t) (x,y)) ||  (afogou ((ter, obs):t) (x,y))
----------------------------------------------------------------------------------------------------------------------
{-|
== foradelimites
A função "__'foradelimites'__" é uma função que
dado um "__Int__", um "__[(Terreno, [Obstaculo])]__" e "__Coordenadas__" determina se o Jogador encontra-se
numa posição para lá dos limites do mapa, devolvendo um Booleano caso seja verdade.

Podemos assim definir "__'foradelimites'__" da seguinte maneira:

=== Definição dada
@
foradelimites :: Int -> [(Terreno, [Obstaculo])] -> Coordenadas -> Bool
foradelimites larg terobs (x,y) | (x < 0) || (x >= larg) || (y < 0) || (y >= (length terobs)) = True
                                | otherwise                                                   = False 
@
-}
foradelimites :: Int -> [(Terreno, [Obstaculo])] -> Coordenadas -> Bool
foradelimites larg terobs (x,y) = (x < 0) || (x >= larg) || (y < 0) || (y >= (length terobs)) 
{-|
== atropelado
A função "__'atropelado'__" é uma função que
dado um "[(Terreno, [Obstaculo])]", determina se o Jogador encontra-se
na mesma posição que um "__'Carro'__" e devolvendo um Booleano caso seja verdade.

Podemos assim definir "__'atropelado'__" da seguinte maneira:

=== Definição dada
@
atropelado :: [(Terreno, [Obstaculo])] -> Coordenadas -> Bool
atropelado ((ter, obs):t) (x,y) = ((snd (((ter , obs):t) !! y)) !! x) == Carro 
@
-}
----------------------------------------------------------------------------------------------------------------------
atropelado :: [(Terreno, [Obstaculo])] -> Coordenadas -> Bool
atropelado ((ter, obs):t) (x,y) = ((snd (((ter , obs):t) !! y)) !! x) == Carro  
{-|
== afogou
A função "__'afogou'__" é uma função que
dado um "__[(Terreno, [Obstaculo])]__" e "__Coordenadas__" determina se o Jogador encontra-se
num Nenhum dentro de um "__'Rio'__", afogando-se assim, devolvendo um Booleano caso seja verdade.
Esta apela pela função "__'serario'__" como auxiliar para determinar se se encontra num "__'Rio'__"

Podemos assim definir "__'afogou'__" da seguinte maneira:

=== Definição dada
@
afogou :: [(Terreno, [Obstaculo])] -> Coordenadas -> Bool
afogou terobs (x,y) = serario (fst (terobs !! y)) && (((snd (terobs !! y)) !! x) == Nenhum)
@
-}
----------------------------------------------------------------------------------------------------------------------
afogou :: [(Terreno, [Obstaculo])] -> Coordenadas -> Bool
afogou terobs (x,y) = serario (fst (terobs !! y)) && (((snd (terobs !! y)) !! x) == Nenhum)
{-|
== serario
A função "__'serario'__" é uma função que
dado um "__'Terreno'__" e determina se o Jogador encontra-se
num "__'Rio'__", devolvendo um Booleano caso seja verdade.

Podemos assim definir "__'serario'__" da seguinte maneira:

=== Definição dada
@
serario :: Terreno -> Bool
serario (Rio _) = True
serario (_)     = False
@
-}
----------------------------------------------------------------------------------------------------------------------
serario :: Terreno -> Bool
serario (Rio _) = True
serario (_)     = False