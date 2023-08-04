{- |
Module      : Tarefa2_2022li1g124
Description : Geração contínua de um mapa
Copyright   : Frederico Cunha Afonso <a104001@alunos.uminho.pt>

Módulo para a realização da Tarefa 2 do projeto de LI1 em 2022/23.
-}
module Tarefa2_2022li1g124 where

import LI12223
import System.Random
import Data.List
------------------------------------------------------------------------------------------
{-|
== estendeMapa
A função __'estendeMapa'__ irá analisar o __'Mapa'__ que lhe foi submetido 
e, com o valor Inteiro recebido (dentro do intervalo [1,100]) irá devolver
o mesmo 'Mapa' recebido, mas adicionará uma "linha" "pseudo-aleatoriamente"
usando o Inteiro recebido como recurso (irá devolver o mesmo 'Mapa', mas
adicionando um par de 'Terreno' e lista de Obstáculos)

Podemos assim definir __'estendeMapa'__ da seguinte maneira:

=== Definição dada
@
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l []) x = Mapa l [(Relva, primlin l [])]  
estendeMapa (Mapa l terobs) x 
      = let proxTer = proximoTerrenoVel x (Mapa l terobs)
            proxObs = proximoObstaculo x l ((proxTer, []):(terobs))
        in Mapa l (((proxTer), (proxObs)):terobs)
@

@
    __:: 'Mapa'__  assume-se qualquer __Mapa__ a esta função
    __-> Int__   iremos tambem providenciar-lhe um número qualquer
                 que será usado para escolher um 'Terreno' qualquer
                 e uma lista de Obstáculos qualquer (tudo aleatóriamente)
    __-> 'Mapa'__  irá devolver um mesmo __Mapa__, com uma "linha adicionada"
                 (adicionando um par 'Terreno' | Lista de Obstáculos à cabeça
                 da lista de pares)
@           

Caso o 'Mapa' dado tenha um conjunto vazio de 'Terreno's | Lista de Obstáculos,
a função irá devolver um 'Terreno' "__'Relva'__" com uma lista de Obstáculos
preenchida de Obstáculos "__'Nenhum'__", baseando-me no 'Jogo' original
(iremos evitar que o 'Jogador' comece num "__'Rio' __" na mesma coordenada de um 
"__'Nenhum'__", ou numa "__'Estrada'__" na mesma coordenada de um "__Carro__").
Usando uma função auxiliar "primlin" 

Caso contrário, irá criar uma nova "linha" com auxílio das funções
"__'proximoTerrenoVel'__" (para determinar o próximo 'Terreno') e
"__'proximoObstaculo'__" (para determinar uma nova lista de Obstáculos)


== Exemplo (para um 'Mapa' sem 'Terreno's ou Obstáculos)
>>> estendeMapa (Mapa 6 []) 10
Mapa 6 [(Relva,[Nenhum,Nenhum,Nenhum,Nenhum,Nenhum,Nenhum])]

-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
estendeMapa :: Mapa -> Int -> Mapa
estendeMapa (Mapa l []) x = Mapa l [(Relva, primlin l [])]  
estendeMapa (Mapa l terobs) x 
      = let proxTer = proximoTerrenoVel x (Mapa l terobs)
            proxObs = proximoObstaculo x l ((proxTer, []):(terobs))
        in Mapa l (((proxTer), (proxObs)):terobs)
---------------------------------------------------------------------------------------------------
{-|
== primlin
A função __'primlin'__ é uma função "cega" por assim dizer.
__'primlin'__ aceita apenas a 'Largura' de um 'Mapa' e uma lista
de obstáculos "qualquer", que, recursivamente, será preenchida
por Obstáculos "__'Nenhum'__" até ter um comprimento (length)
igual à 'Largura' do 'Mapa'

Podemos assim definir __'primlin'__ da seguinte maneira:

=== Definição dada
@
primlin :: Int -> [Obstaculo] -> [Obstaculo]
primlin l t | length t == l = t
            | otherwise     = primlin l ((Nenhum):t)
@

@
    __:: Int__          recebe assim a 'Largura' do 'Mapa'
    __-> ['Obstaculo']__  irá receber uma lista de Obstáculos (vinda da função estendeMapa
                        que apenas lhe dá uma lista vazia para, propositadamente, só ser
                        capaz de criar uma lista de obstáculos "__'Nenhum'__" recursivamente)
    __-> ['Obstaculo']__  irá devolver uma lista de Obstáculos (respetivamente, uma lista preenchida
                        de Obstáculos "__'Nenhum'__"),
@           

-}
------------------------------------------------------
primlin :: Int -> [Obstaculo] -> [Obstaculo]
primlin l t 
      | length t == l 
            = t
      | otherwise     
            = primlin l ((Nenhum):t)
{-|
== proximoTerrenoVel
A função "__'proximoTerrenoVel'__" é uma função que,
auxiliada pelas funções "__'randomizer'__" e "__'proximoTerrenoVel'__",
determina qual será o próximo 'Terreno' de uma lista de possíveis 'Terreno's.

Através da função "__'randomizer'__", dependendo do número resultante ([0,1,2]),
"__'proximoTerrenoVel'__" irá escolher a cabeça, o último membro ou até a cabeça
da tail da lista de possíveis 'Terreno's.

Tal lista de possíveis 'Terreno's é dada pela função auxiliar "__'proximoTerrenoVel'__"


Podemos assim definir "__'proximoTerrenoVel'__" da seguinte maneira:

=== Definição dada
@
proximoTerrenoVel :: Int -> Mapa -> Terreno
proximoTerrenoVel x (Mapa l terobs) 
      | random1 == 2 
            = head (terrenosVels x (terreno))
      | random1 == 1 
            = head (tail (terrenosVels x (terreno)))
      | random1 == 0 
            = last (terrenosVels x (terreno))
            where terreno = proximosTerrenosPossiveis (Mapa l (reverse terobs)) 0 0 0
                  random1 = (mod (head(randomizer x)) 3)
@

@
    __:: Int__     recebe assim o número dado pelo Usuário (o dado aleatório)
    __-> 'Mapa'__    recebe também o 'Mapa' do 'Jogo' presente para a "__'proximoTerrenoVel'__"
                   conseguir perceber quais os __Proximos 'Terreno's Validos__
    __-> 'Terreno'__ irá devolver um 'Terreno' possível decidido pela função "__'randomizer'__"
                   e escolhido de uma lista providenciada pela "__'proximoTerrenoVel'__"
@           
-}
-------------------------------------------proximoObstaculo--------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
proximoTerrenoVel :: Int -> Mapa -> Terreno
proximoTerrenoVel x (Mapa l terobs) 
      | random1 == 2 
            = head (terrenosVels x (terreno))
      | random1 == 1 
            = head (tail (terrenosVels x (terreno)))
      | random1 == 0 
            = last (terrenosVels x (terreno))
            where terreno = proximosTerrenosPossiveis (Mapa l (reverse terobs)) 0 0 0
                  random1 = (mod (head(randomizer x)) 3)
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
== terrenosVels
A função "__'terrenosVels'__" é uma função que, dado um Int e uma lista de 'Terreno's 
devolve uma lista de 'Terreno's possíveis com auxílio da função
"__'proximosTerrenosPossiveis'__", e com 'Velocidade's que variam do (-3) ao 3


Podemos assim definir "__'terrenosVels'__" da seguinte maneira:

=== Definição dada
@
terrenosVels :: Int -> [Terreno] -> [Terreno]
terrenosVels _ [] = []
terrenosVels x (Relva:t) = (Relva):(terrenosVels x t)
terrenosVels x ((Estrada k):t) | k == 0    = (Estrada ((posneg x) * (velrandom x))):(terrenosVels x t)
                               | otherwise = (Estrada ((k) * (velrandom x))):(terrenosVels x t)
terrenosVels x ((Rio k):t) | k == 0    = (Rio ((posneg x) * (velrandom x))):(terrenosVels x t)
                           | otherwise = (Rio ((k) * (velrandom x))):(terrenosVels x t)
velrandom x = (mod (head(randomizer (x+207))) 3) + 1
@

@
    __:: Int__      recebe um 'Mapa' para percebe quais os próximos 'Terreno's possíveis 
    __-> [Terreno]__ irá receber uma lista de 'Terreno's com 'Velocidade's que variam do
                     (-1) ao 1,
    __-> [Terreno]__ irá devolver uma lista de 'Terreno's possíveis decidido pela 
                     função "__'proximosTerrenosPossiveis'__",
@           
-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
terrenosVels :: Int -> [Terreno] -> [Terreno]
terrenosVels _ [] = []
terrenosVels x (Relva:t) = (Relva):(terrenosVels x t)
terrenosVels x ((Estrada k):t) | k == 0    = (Estrada ((posneg x) * (velrandom x))):(terrenosVels x t)
                               | otherwise = (Estrada ((k) * (velrandom x))):(terrenosVels x t)
terrenosVels x ((Rio k):t) | k == 0    = (Rio ((posneg x) * (velrandom x))):(terrenosVels x t)
                           | otherwise = (Rio ((k) * (velrandom x))):(terrenosVels x t)
velrandom x = (mod (head(randomizer (x+207))) 3) + 1
-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
== proximosTerrenosPossiveis
A função "__'proximosTerrenosPossiveis'__" é uma função que,
dada um 'Mapa', e três Inteiros (denominadamente, três 0's) 
devolve uma lista de 'Terreno's

Podemos assim definir "__'proximosTerrenosPossiveis'__" da seguinte maneira:

=== Definição dada
@
proximosTerrenosPossiveis :: Mapa -> Int -> Int -> Int -> [Terreno]
proximosTerrenosPossiveis (Mapa larg (((Rio vel1), obs):[])) _ _ rio | (rio < 3) 
                                                                        = [Rio ((-1)*(signum vel1)), Estrada ((-1)*(signum vel1)), Relva]
                                                                     | otherwise               
                                                                        = [Estrada ((-1)*(signum vel1)), Relva]

proximosTerrenosPossiveis (Mapa larg (((Estrada vel1), obs):[])) estr _ _ | (estr < 3) 
                                                                              = [Rio ((-1)*(signum vel1)), Estrada ((-1)*(signum vel1)), Relva]
                                                                          | otherwise               
                                                                              = [Rio ((-1)*(signum vel1)), Relva]

proximosTerrenosPossiveis (Mapa larg []) _ relv _ | (relv > 4)
                                                      = [Rio 0, Estrada 0]                      
                                                  | otherwise 
                                                      = [Rio 0, Estrada 0, Relva]

proximosTerrenosPossiveis (Mapa larg ((Estrada vel1, obs):t)) estr relv rio 
      = proximosTerrenosPossiveis (Mapa larg t) (estr+1) 0 0

proximosTerrenosPossiveis (Mapa larg ((Relva, obs):t)) estr relv rio       
      = proximosTerrenosPossiveis (Mapa larg t) 0 (relv+1) 0

proximosTerrenosPossiveis (Mapa larg ((Rio vel1, obs):t)) estr relv rio     
      = proximosTerrenosPossiveis (Mapa larg t) 0 0 (rio+1)

@

@
    __:: 'Mapa'__      recebe um 'Mapa' para percebe quais os próximos 'Terreno's possíveis 
    __-> Int__       recebe um Int relacionado há quantidade de "__'Estrada's__" seguidas na lista
    __-> Int__       recebe um Int relacionado há quantidade de "__'Relva's__" seguidas na lista
    __-> Int__       recebe um Int relacionado há quantidade de "__''Rio's__" seguidas na lista
    __-> ['Terreno']__ devolve uma lista de 'Terreno's possíveis 
@           

Baseando-se nas condições propostas na 1ª tarefa do projeto,
a função "__'proximosTerrenosPossiveis'__" irá recursivamente ver
todos os "__'Terrenos'__" de todos os pares da lista e irá
concluir quais os 'Terreno's possíveis para continuar o 'Mapa'
(tendo em especial atenção a 2ª e 7ª condição da tarefa 1)
-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
proximosTerrenosPossiveis :: Mapa -> Int -> Int -> Int -> [Terreno]
proximosTerrenosPossiveis (Mapa larg (((Rio vel1), obs):[])) _ _ rio | (rio < 3) 
                                                                        = [Rio ((-1)*(signum vel1)), Estrada ((-1)*(signum vel1)), Relva]
                                                                     | otherwise               
                                                                        = [Estrada ((-1)*(signum vel1)), Relva]

proximosTerrenosPossiveis (Mapa larg (((Estrada vel1), obs):[])) estr _ _ | (estr < 3) 
                                                                              = [Rio ((-1)*(signum vel1)), Estrada ((-1)*(signum vel1)), Relva]
                                                                          | otherwise               
                                                                              = [Rio ((-1)*(signum vel1)), Relva]

proximosTerrenosPossiveis (Mapa larg []) _ relv _ | (relv > 4)
                                                      = [Rio 0, Estrada 0]                      
                                                  | otherwise 
                                                      = [Rio 0, Estrada 0, Relva]

proximosTerrenosPossiveis (Mapa larg ((Estrada vel1, obs):t)) estr relv rio 
      = proximosTerrenosPossiveis (Mapa larg t) (estr+1) 0 0

proximosTerrenosPossiveis (Mapa larg ((Relva, obs):t)) estr relv rio       
      = proximosTerrenosPossiveis (Mapa larg t) 0 (relv+1) 0

proximosTerrenosPossiveis (Mapa larg ((Rio vel1, obs):t)) estr relv rio     
      = proximosTerrenosPossiveis (Mapa larg t) 0 0 (rio+1)

{-|
== proximoObstaculo
A função "__'proximoObstaculo'__" é uma função que,
dada um 'Mapa', e três Inteiros (denominadamente, três 0's) 
devolve uma lista de 'Terreno's

Podemos assim definir "__'proximoObstaculo'__" da seguinte maneira:

=== Definição dada
@
proximoObstaculo :: Int -> Int -> [(Terreno, [Obstaculo])] -> [Obstaculo]
proximoObstaculo x l ((ter, obs):t) 
      | elem Tronco obs && length obs == l               = bomRio obs
      | ter == Relva && length obs == l                  = boaRelva ((ter,obs):t) x
      | proximosObstaculosValidoss l ((ter, obs):t) == [] = obs
      | even (head(tail (randomizer x)))                 = (proximoObstaculo ((x*3)+1) l ((ter, (obs ++ [proxobst1])):t))
      | otherwise                                        = (proximoObstaculo ((x*3)+1) l ((ter, (obs ++ [proxobst2])):t))
                                                            where proxobst1 = last (proximosObstaculosValidoss l ((ter, obs):t))
                                                                  proxobst2 = head (proximosObstaculosValidoss l ((ter, obs):t))
@

@
    __:: Int__                         recebe um Int dado inicialmente pelo Usuário que servirá para fins de "aleatoriedade"
    __-> Int__                         recebe um Int relacionado há 'Largura' do 'Mapa'
    __-> [(Terreno, ['Obstaculo'])]__    recebe uma lista de tuplos de pares 'Terreno' | lista de Obstáculos para perceber com que 'Terreno' está a tratar 
    __-> ['Obstaculo']__                 devolve uma lista de "__'Obstáculos'__" certa
@           

"__'proximoObstaculo'__"irá usar as funções auxiliares
"__'randomizer x'__" para decidir quais os "__'Obstáculos'__"
irá adicionar para a lista final de obstáculos, vindos estes
da função auxiliar "__'proximosObstaculosValidoss'__"
-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
proximoObstaculo :: Int -> Int -> [(Terreno, [Obstaculo])] -> [Obstaculo]
proximoObstaculo x l ((ter, obs):t) 
      | elem Tronco obs && length obs == l               = bomRio obs
      | ter == Relva && length obs == l                  = boaRelva ((ter,obs):t) x
      | proximosObstaculosValidoss l ((ter, obs):t) == [] = obs
      | even (head(tail (randomizer x)))                 = (proximoObstaculo ((x*3)+1) l ((ter, (obs ++ [proxobst1])):t))
      | otherwise                                        = (proximoObstaculo ((x*3)+1) l ((ter, (obs ++ [proxobst2])):t))
                                                            where proxobst1 = last (proximosObstaculosValidoss l ((ter, obs):t))
                                                                  proxobst2 = head (proximosObstaculosValidoss l ((ter, obs):t))
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
== proximosObstaculosValidoss 
A função "__'proximosObstaculosValidoss'__" é uma função que,
dado um Inteiro relacionado à 'Largura' do 'Mapa', e um par 
Terreno | lista de 'Obstaculo's, deduz uma lista de possíveis 
obstáculos com atenção às condições da primeira tarefa

Podemos assim definir "__'proximosObstaculosValidoss'__" da seguinte maneira:

=== Definição dada
@

proximosObstaculosValidoss :: Int -> [(Terreno, [Obstaculo])] -> [Obstaculo]
proximosObstaculosValidoss larg ((_, obs):t) | larg == length obs = []
proximosObstaculosValidoss larg ((Relva, obs):((Rio vel),obs2):t)
      | (vel < 0 && (not (elem Nenhum (take ((round((fromIntegral larg)/2))) obs)))) || (vel > 0 && length obs < round((fromIntegral larg)/2) &&  (not (elem Nenhum obs)))
            = [Nenhum]
      | otherwise                                                                                                                            
            = [Nenhum, Arvore]

proximosObstaculosValidoss larg ((Relva, obs):((Estrada vel),obs2):t)
      | (vel < 0 && (not (elem Nenhum (take ((round((fromIntegral larg)/2))) obs)))) || (vel > 0 && length obs < round((fromIntegral larg)/2) &&  (not (elem Nenhum obs)))
            = [Nenhum]
      | otherwise                                                                                                                            
            = [Nenhum, Arvore]
proximosObstaculosValidoss larg ((Relva, obs):t) = [Nenhum, Arvore]

proximosObstaculosValidoss larg ((Rio vel, obs):t)
      |length obs == larg 
            = []
      |((drop (length obs - 2) obs) == [Nenhum,Tronco]) && (not (tronquinho obs 0)) 
      || (obs == [Tronco])
      || (obs /= []) && (not (elem Tronco (tail obs)) && ((length obs) >= larg-3)) 
            = [Tronco]
      |(tronquinho obs 0)
      || (((length obs) > larg-2) && (head obs == Tronco) && (tronquinho obs 0)) 
      || (((length obs) > larg-2) && (head obs == Nenhum)) 
            = [Nenhum]      
      |otherwise 
            = [Nenhum,Tronco]
            where tronquinho obs k |(obs == []) || (head obs == Nenhum && last obs == Nenhum) = k> 4
                                   |head obs == Tronco                                        = tronquinho (tail obs) (k+1)
                                   |last obs == Tronco                                        = tronquinho (init obs) (k+1)
                                   |otherwise                                                 = False


proximosObstaculosValidoss larg ((Estrada vel, obs):t) 
      | (restaUmObs && (obs !! 1 == Carro)) || (restaDoisObs && head obs == Carro) || drop ((length obs) -3) obs == [Carro,Carro,Carro] || (drop ((length obs) -2) obs == [Carro,Nenhum])
            = [Nenhum]
      | otherwise                                                                                                                                
            = [Nenhum,Carro]
            where restaDoisObs = (larg - (length obs) <= 2)  
                  restaUmObs   = (larg - (length obs) <= 1)    

@
@
    __:: Int__                        recebe um Int relacionado à 'Largura'
    __-> [('Terreno', ['Obstaculo'])]__   recebe uma lista de tuplos de pares 'Terreno' | lista de Obstáculos para perceber com que 'Terreno' está a tratar
                                      para perceber que "__'Obstáculos'__" pode devolver, tendo em conta as condições da 1ª tarefa 
    __-> ['Obstaculo']__                devolve uma lista de "__'Obstáculos'__" possíveis

@           

Baseando-se nas condições propostas na 1ª tarefa do projeto,
a função "__'proximosObstaculosValidoss'__" irá recursivamente ver
todos os "__'Obstáculos'__" da lista e irá determinar uma lista
de "__'Obstáculos'__" adequada ao "__'Terreno'__" dado,
auxiliada pela função "__'proximosObstaculosValidoss'__"
concluir quais os 'Terreno's possíveis para continuar o 'Mapa'
(tendo em especial atenção a 1ª,3ª,4ª,5ª e 6ª condição da tarefa 1)


-}
proximosObstaculosValidoss :: Int -> [(Terreno, [Obstaculo])] -> [Obstaculo]
proximosObstaculosValidoss larg ((_, obs):t) | larg == length obs = []
                                             | length obs == (larg-1) && not (elem Nenhum obs) = [Nenhum]
proximosObstaculosValidoss larg ((Relva, obs):((Rio vel),obs2):t)
      | (vel < 0 && (not (elem Nenhum (take ((round((fromIntegral larg)/2))) obs)))) || (vel > 0 && length obs < round((fromIntegral larg)/2) &&  (not (elem Nenhum obs)))
            = [Nenhum]
      | otherwise                                                                                                                            
            = [Nenhum, Arvore]

proximosObstaculosValidoss larg ((Relva, obs):((Estrada vel),obs2):t)
      | (vel < 0 && (not (elem Nenhum (take ((round((fromIntegral larg)/2))) obs)))) || (vel > 0 && length obs < round((fromIntegral larg)/2) &&  (not (elem Nenhum obs)))
            = [Nenhum]
      | otherwise                                                                                                                            
            = [Nenhum, Arvore]
proximosObstaculosValidoss larg ((Relva, obs):t) = [Nenhum, Arvore]

proximosObstaculosValidoss larg ((Rio vel, obs):t)
      |length obs == larg 
            = []
      |((drop (length obs - 2) obs) == [Nenhum,Tronco]) && (not (tronquinho obs 0)) 
      || (obs == [Tronco])
      || (obs /= []) && (not (elem Tronco (tail obs)) && ((length obs) >= larg-3)) 
            = [Tronco]
      |(tronquinho obs 0)
      || (((length obs) > larg-2) && (head obs == Tronco) && (tronquinho obs 0)) 
      || (((length obs) > larg-2) && (head obs == Nenhum)) 
            = [Nenhum]      
      |otherwise 
            = [Nenhum,Tronco]
            where tronquinho obs k |(obs == []) || (head obs == Nenhum && last obs == Nenhum) = k> 4
                                   |head obs == Tronco                                        = tronquinho (tail obs) (k+1)
                                   |last obs == Tronco                                        = tronquinho (init obs) (k+1)
                                   |otherwise                                                 = False


proximosObstaculosValidoss larg ((Estrada vel, obs):t) 
      | (restaUmObs && (obs !! 1 == Carro)) || (restaDoisObs && head obs == Carro) || drop ((length obs) -3) obs == [Carro,Carro,Carro] || (drop ((length obs) -2) obs == [Carro,Nenhum])
            = [Nenhum]
      | otherwise                                                                                                                                
            = [Nenhum,Carro]
            where restaDoisObs = (larg - (length obs) <= 2)  
                  restaUmObs   = (larg - (length obs) <= 1)  
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
== bomRio
A função "__'bomRio'__" é uma função que, recebendo uma lista de 'Obstaculo's,
verifica recursivamente se neste existem 'Tronco's, e caso isto seja verdadeiro,
remove os 'Tronco's que não estão acompanhados por pelo menos outro 'Tronco',
tendo em conta, que posteriormente, ao querer que a parte gráfica descreva
um "deslocamento suave", ultrapassar um 'Rio' pode ser mais difícil com apenas
1 'Tronco'

Podemos assim definir "__'bomRio'__" da seguinte maneira:

=== Definição dada
@
bomRio :: [Obstaculo] -> [Obstaculo]
bomRio obs |length (elemIndices Nenhum obs) < 2 = obs
           |(head (tail (elemIndices Nenhum obs)))-(head (elemIndices Nenhum obs)) == 2 = (take (1+ head (elemIndices Nenhum obs)) obs) ++ [Nenhum] ++ (bomRio (drop (2+ head (elemIndices Nenhum obs)) obs)) 
           | otherwise = (take (1+ head (elemIndices Nenhum obs)) obs) ++ (bomRio (drop (1+ head (elemIndices Nenhum obs)) obs))
@
@
    __:: ['Obstaculo']__   recebe uma lista de 'Obstaculo's
    __-> ['Obstaculo']__   devolve uma lista de 'Obstaculo's
@           
-}


bomRio :: [Obstaculo] -> [Obstaculo]
bomRio obs |length (elemIndices Nenhum obs) < 2 = obs
           |(head (tail (elemIndices Nenhum obs)))-(head (elemIndices Nenhum obs)) == 2 = (take (1+ head (elemIndices Nenhum obs)) obs) ++ [Nenhum] ++ (bomRio (drop (2+ head (elemIndices Nenhum obs)) obs)) 
           | otherwise = (take (1+ head (elemIndices Nenhum obs)) obs) ++ (bomRio (drop (1+ head (elemIndices Nenhum obs)) obs))

{-|
== boaRelva
A função "__'boaRelva'__" ,tal como a função __'bomRio'__, verifica
se a lista de 'Relva's recebida tem pelo menos "um caminho passável",
caso contrário, chama a função __'actuallyBoaRelva'__ e reescreve
a lista de tuplos pares 'Terreno' |lista de 'Obstaculo's

Podemos assim definir "__'boaRelva'__" da seguinte maneira:

=== Definição dada
@
boaRelva :: [(Terreno,[Obstaculo])] -> Int -> [Obstaculo]
boaRelva ((Relva,obs):t) x | (elem (replicate ((length (numRelvas ((Relva, obs):t)))) Nenhum) (obsRelvas(numRelvas ((Relva, obs):t)))) = obs
                           | otherwise = actuallyBoaRelva (relvLinhas (numRelvas ((Relva,obs):t))) x 

@
@
    __:: [('Terreno',['Obstaculo'])]__   recebe uma lista de tuplos pares 'Terreno' | lista de 'Obstaculo's
    __-> Int__ recebe um Inteiro usado para aleatoriedade
    __-> ['Obstaculo']__   devolve uma lista de 'Obstaculo's
@           
-}

boaRelva :: [(Terreno,[Obstaculo])] -> Int -> [Obstaculo]
boaRelva ((Relva,obs):t) x | (elem (replicate ((length (numRelvas ((Relva, obs):t)))) Nenhum) (obsRelvas(numRelvas ((Relva, obs):t)))) = obs
                           | otherwise = actuallyBoaRelva (relvLinhas (numRelvas ((Relva,obs):t))) x 
actuallyBoaRelva :: [[Obstaculo]] -> Int -> [Obstaculo]
actuallyBoaRelva (obs:t) x | length t > 1 = (take (randomlyColunaNenhum) obs) ++ [Nenhum] ++ (drop (randomlyColunaNenhum+1) obs) 
                           | otherwise    = (take randomlyNenhum obs) ++ [Nenhum] ++ (drop (randomlyNenhum+1) obs)
                              where Just y = elemIndex (replicate (length t) Nenhum) (relvCols t)
                                    randomlyColunaNenhum = (elemIndices (replicate (length t) Nenhum) (relvCols t)) !! (mod x ((length ((elemIndices (replicate (length t) Nenhum) (relvCols t))))))
                                    randomlyNenhum = (elemIndices Nenhum (head t)) !! (mod x ((length ((elemIndices Nenhum (head t))))))
------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

--------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
== posneg
A função "__'posneg'__" é uma função que, baseando-se
na randomizer, decide se resulta num 1 ou num (-1),
se o randomizer obtido pelo input do Usuário for par
ou ímpar

Podemos assim definir "__'posneg'__" da seguinte maneira:

=== Definição dada
@
posneg :: Int -> Int
posneg x | odd (last (randomizer x)) = (-1)
         | otherwise = 1
@
@
    __:: Int__   recebe um Int relacionado à "aleatoriedade" da criação de uma nova linha 
    __-> Int__   devolve um Int (1 ou (-1))
@           
-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
posneg :: Int -> Int
posneg x | odd (head(tail (randomizer x))) 
            = (-1)
         | otherwise 
            = 1
{-|
== randomizer
A função "__'randomizer'__" é uma função que, 
dado um Int pelo Usuário, devolve um número
"pseudo-aleatório"

Podemos assim definir "__'randomizer'__" da seguinte maneira:

=== Definição dada
@
randomizer :: Int -> [Int]
randomizer x 
      = take 5 $ randoms (mkStdGen x)
@
@
    __:: Int__     recebe um Int relacionado à "aleatoriedade" da criação de uma nova linha 
    __-> [Int]__   devolve uma lista de 2 números "pseudo-aleatórios"
@
-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
randomizer :: Int -> [Int]
randomizer x 
      = take 5 $ randoms (mkStdGen x)




-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|
== numRelvas
A função "__'numRelvas'__" é uma função que, 
dado um tuplo de pares 'Terreno' | lista de 'Obstaculo's
Devolve apenas a lista contida na original, que apenas
contém 'Relva's

=== Definição dada
@
numRelvas :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
numRelvas ((Relva,obs):t) 
      = (Relva,obs):(numRelvas t)
numRelvas _ 
      = []
@
@
    __:: [('Terreno',['Obstaculo'])]__     recebe uma lista de tuplos de pares 'Terreno' | lista de 'Obstaculo's
    __-> [('Terreno',['Obstaculo'])]__   devolve uma lista de tuplos de pares 'Terreno' | lista de 'Obstaculo's
@
-}
-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

numRelvas :: [(Terreno,[Obstaculo])] -> [(Terreno,[Obstaculo])]
numRelvas ((Relva,obs):t) 
      = (Relva,obs):(numRelvas t)
numRelvas _ 
      = []

{-|
== obsRelvas
A função "__'obsRelvas'__" é uma função que, 
se considerarmos a lista de 'Obstaculo's numa matriz,
torna esta mesma matriz numa matriz transposta, com
auxílio das funções __'relvLinhas'__ e __'relvCols'__

=== Definição dada
@
obsRelvas :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]
obsRelvas terobs 
      = relvCols (relvLinhas terobs)
@
@
    __:: [('Terreno',['Obstaculo'])]__     recebe uma lista de tuplos de pares 'Terreno' | lista de 'Obstaculo's
    __-> [['Obstaculo']]__   devolve uma lista de listas de 'Obstaculo's
@
-}

obsRelvas :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]
obsRelvas terobs 
      = relvCols (relvLinhas terobs)

{-|
== relvLinhas
A função "__'relvLinhas'__" é uma função que, 
dada uma lista de tuplos de pares 'Terreno' | lista de 'Obstaculo's,
devolve uma lista de listas de 'Obstaculo's

=== Definição dada
@
relvLinhas :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]
relvLinhas []    
      = []
relvLinhas terobs 
      = (map snd terobs)
@
@
    __:: [('Terreno',['Obstaculo'])]__     recebe um lista de tuplos de pares 'Terreno' | lista de 'Obstaculo's
    __-> [['Obstaculo']]__   devolve uma lista de listas de 'Obstaculo's
@
-}

relvLinhas :: [(Terreno,[Obstaculo])] -> [[Obstaculo]]
relvLinhas []    
      = []
relvLinhas terobs 
      = (map snd terobs)

{-|
== relvCols
A função "__'relvCols'__" é uma função que, 
dada uma lista de listas de 'Obstaculo's,
considerando-a como uma matriz,
devolve a matriz transposta da mesma

=== Definição dada
@
relvCols :: [[Obstaculo]] -> [[Obstaculo]]
relvCols [] 
      = []
relvCols ([]:t) 
      = relvCols t
relvCols obsObs 
      = (map head obsObs):(relvCols (map tail obsObs))

@
@
    __:: [['Obstaculo']]__   recebe uma lista de listas de 'Obstaculo's
    __-> [['Obstaculo']]__   devolve uma lista de listas de 'Obstaculo's
@
-}


relvCols :: [[Obstaculo]] -> [[Obstaculo]]
relvCols [] 
      = []
relvCols ([]:t) 
      = relvCols t
relvCols obsObs 
      = (map head obsObs):(relvCols (map tail obsObs))

-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|
== velTer1
A função "__'velTer1'__" é uma função que, 
dado um 'Terreno', devolve a 'Velocidade' deste

=== Definição dada
@

velTer1 :: Terreno -> Int
velTer1 (Estrada vel) = vel
velTer1 (Rio vel) = vel
velTer1 (Relva) = 0

@
@
    __:: 'Terreno'__   recebe um 'Terreno'
    __-> Int__   devolve um Int
@
-}

velTer1 :: Terreno -> Int
velTer1 (Estrada vel) = vel
velTer1 (Rio vel) = vel
velTer1 (Relva) = 0

proximosTerrenosValidos ((Mapa l (terobs))) = proximosTerrenosPossiveis ((Mapa l (reverse terobs))) 0 0 0
proximosObstaculosValidos x to = proximosObstaculosValidoss x [to]