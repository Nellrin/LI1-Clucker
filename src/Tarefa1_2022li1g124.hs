{- |
Module      : Tarefa1_2022li1g124
Description : Validação de um mapa
Copyright   : Frederico Cunha Afonso <a104001@alunos.uminho.pt>

Módulo para a realização da Tarefa 1 do projeto de LI1 em 2022/23.
-}
module Tarefa1_2022li1g124 where

import LI12223
import Data.List
{-| 
== mapaValido
A função __'mapaValido'__ irá analisar o __'Mapa'__ que lhe for submetido 
e dará uma avaliação binária do mesmo, avaliando-o tendo em conta vários 
critérios que foram apresentados no enunciado providenciado pelos docentes.

Para facilitar este processo, __'mapaValido'__ divide-se em 7 verificações 
diferentes, que em si abordam diferentes condições que o mapa do jogo terá 
de ter em conta para ser avaliado como /True/ (Válido).

Podemos assim definir __'mapaValido'__ da seguinte maneira:

=== Definição dada
@
mapaValido :: Mapa -> Bool
mapaValido m = verificacao1 m && verificacao2 m  && verificacao3 m && verificacao4 m && verificacao5 m && verificacao6 m && verificacao7 m
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma a validez do mapa providenciado
@           

Para a função estar correta, cada verificação acima irá devolver um Booleano 
que irá confirmar uma certa condição que fora inicialmente apresentada no 
enunciado

-}
-------------------------------------------------------------------------------------------------------------
mapaValido :: Mapa -> Bool
mapaValido m = verificacao1 m && verificacao2 m  && verificacao3 m && verificacao4 m && verificacao5 m && verificacao6 m && verificacao7 m
{-|
== verificacao1
A função __'verificacao1'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 1ª condição
apresentada, "Não existem obstáculos em terrenos impróprios, e.g. troncos em es-
tradas ou relvas, árvores em rios ou estradas, etc."

Podemos assim definir __'verificacao1'__ da seguinte maneira:

=== Definição dada
@
verificacao1 :: Mapa -> Bool 
verificacao1 (Mapa l []) = True
verificacao1 (Mapa l ((Rio _, obs):t))     = ((not (elem Arvore obs)) && (not (elem Carro obs))) && verificacao1 (Mapa l t)
verificacao1 (Mapa l ((Estrada _, obs):t)) = ((not (elem Arvore obs)) && (not (elem Tronco obs))) && verificacao1 (Mapa l t)
verificacao1 (Mapa l ((Relva, obs):t))     = ((not (elem Tronco obs)) && (not (elem Carro obs))) && verificacao1 (Mapa l t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma se o __'Mapa'__ 
                 dado contém obstáculos em terrenos impróprios
@           

Aqui, __'verificacao1'__ irá avaliar a cabeça de cada lista, analisando se "não contém" obstáculos que não 
lhe pertencem, continuando até ao fim da lista.

== Exemplo (para um terreno "Rio _" de 3 de largura)
>>> not (elem Arvore [Tronco,Nenhum,Nenhum])
True

>>> not (elem Arvore [Carro, Nenhum,Nenhum]) 
True

>>> not (elem Arvore [Nenhum,Tronco,Arvore]) 
False

Aplicando este raciocínio a outros obstáculos que não pertencem ao terreno "Rio _", obteremos uma função
que deteta obstáculos que não deviam pertencer em qualquer "Rio _", mas que de facto estão lá presentes
Continuando tal raciocínio para todos os terrenos, temos a função concluída e funcional. 
-}
---------------------------------------------------------------------------------------------------1.
verificacao1 :: Mapa -> Bool 
verificacao1 (Mapa l []) = True
verificacao1 (Mapa l ((Rio _, obs):t))     = ((not (elem Arvore obs)) && (not (elem Carro obs))) && verificacao1 (Mapa l t)
verificacao1 (Mapa l ((Estrada _, obs):t)) = ((not (elem Arvore obs)) && (not (elem Tronco obs))) && verificacao1 (Mapa l t)
verificacao1 (Mapa l ((Relva, obs):t))     = ((not (elem Tronco obs)) && (not (elem Carro obs))) && verificacao1 (Mapa l t)
{-|
== verificacao2
A função __'verificacao2'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 2ª condição
apresentada, "Rios contı́guos têm direcções opostas."

Podemos assim definir __'verificacao2'__ da seguinte maneira:

=== Definição dada
@
verificacao2 :: Mapa -> Bool 
verificacao2 (Mapa larg []) = True
verificacao2 (Mapa larg ((Rio vel1, obs1):(Rio vel2, obs2):t)) | vel1 * vel2 < 0 = verificacao2 (Mapa larg ((Rio vel2, obs2):t))
                                                               | otherwise       = False
verificacao2 (Mapa larg (_:t)) = verificacao2 (Mapa larg t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma se o __'Mapa'__ 
                 dado contém rios contíguos de direções divergentes
@           

Aqui, __'verificacao2'__ irá avaliar os 2 primeiros elementos de cada lista (__h1:h2__:xs), 
analisando se são 2 terrenos "Rio _".Se não o forem, írá deixar a cabeça da lista e irá
analisar o próximo par possível. Caso não haja qualquer par de rios contíguos, a função
diz-se /True/ (Verdadeira). Caso contrário, o produto das velocidades dos dois Rios contíguos, 
terá de ser negativa, pois o produto de qualquer negativo por qualquer positivo
é menor do que 0

== Exemplo (para dois Rios contíguos de uma certa lista)
>>> verificacao2 (Mapa 3 [(Rio 3, [Nenhum,Tronco,Tronco]),(Rio (-1), [Tronco,Nenhum,Tronco])])
True

>>> verificacao2 (Mapa 3 [(Rio 3, [Nenhum,Tronco,Tronco]),(Rio 1, [Tronco,Nenhum,Tronco])])
False

>>> verificacao2 (Mapa 3 [(Relva,Nenhum,Nenhum]), (Rio 3, [Tronco,Tronco]),(Rio (-1), [Nenhum,Tronco])])
True

>>> verificacao2 (Mapa 2 [(Relva,[Nenhum,Nenhum]), (Rio 3, [Nenhum,Tronco]), (Rio 1, [Tronco,Tronco])])
False 
-}
---------------------------------------------------------------------------------------------------2.
verificacao2 :: Mapa -> Bool 
verificacao2 (Mapa larg []) = True
verificacao2 (Mapa larg ((Rio vel1, obs1):(Rio vel2, obs2):t)) | vel1 * vel2 < 0 = verificacao2 (Mapa larg ((Rio vel2, obs2):t))
                                                               | otherwise       = False
verificacao2 (Mapa larg (_:t)) = verificacao2 (Mapa larg t)
{-|
== verificacao3
A função __'verificacao3'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 3ª condição
apresentada, "Troncos têm, no máximo, 5 unidades de comprimento."

Podemos assim definir __'verificacao3'__ da seguinte maneira:

=== Definição dada
@
verificacao3 :: Mapa -> Bool
verificacao3 (Mapa l [])              = True
verificacao3 (Mapa l ((Rio _, []):t)) = verificacao3 (Mapa l t)
verificacao3 (Mapa l ((Rio velrio, obs):t)) | (elem Nenhum obs) && (last obs == Tronco)                         = verificacao3 (Mapa l ((Rio velrio, (last obs:init obs)):t))
                                            | (elem Tronco (head (group obs))) && length (head (group obs)) > 5 = False  
                                            | otherwise                                                         = verificacao3 (Mapa l ((Rio velrio, (tail obs)):t))
verificacao3 (Mapa l ((_, obs):t)) = verificacao3 (Mapa l t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma se o __'Mapa'__ dado contém uma 
                 lista de obstáculos com Troncos de mais de 5 de comprimento
@           

Aqui, __'verificacao3'__ irá procurar na lista de pares TerrenoObstáculos, um par
com Terreno "Rio _" e que deixe qualquer outro "para trás" ou "por avaliar", sendo
que mais nenhum Terreno aceita Troncos, nunca terão mais de 5 troncos, logo, satisfaz
a condição dada.
Ao encontrar tal Terreno, irá "ordenar" a lista de obstáculos de forma a que, se de facto
existir o Obstáculo "__'Nenhum'__" nesta lista e se o último elemento desta for um 
"__'Tronco'__", irá mover o último "__'Tronco'__" para a cabeça da lista. Tal até que o último 
elemento da lista seja um "__'Nenhum'__". Ao ter "ordenado" a lista, usará a função __group__
e criará uma lista de listas.
Com esta nova lista, só teremos de ir de uma ponta da lista à outra, à procura de uma lista
(dentro da nova lista criada), que tenha um elemento "__'Tronco'__" e que tenha um tamanho
superior a 5, sendo que, pela função group, se existe 1 certo obstáculo numa lista da lista
criada, todos os elementos dessa lista em específico serão esse mesmo certo obstáculo.  


== Exemplo (para um Rio qualquer)
>>> verificacao3 (Mapa 8 [(Rio 3, [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum])])
True

>>> group [Nenhum,Tronco,Tronco,Tronco,Tronco,Tronco,Nenhum,Tronco,Nenhum]
[[Nenhum],[Tronco,Tronco,Tronco,Tronco,Tronco],[Nenhum],[Tronco],[Nenhum]]

>>> length [Tronco,Tronco,Tronco,Tronco,Tronco]
5

>>> length [Nenhum]
1

>>> length [Tronco]
1

>>> length [Nenhum]
1

-}
---------------------------------------------------------------------------------------------------
verificacao3 :: Mapa -> Bool
verificacao3 (Mapa l [])              = True
verificacao3 (Mapa l ((Rio _, []):t)) = verificacao3 (Mapa l t)
verificacao3 (Mapa l ((Rio velrio, obs):t)) | (elem Nenhum obs) && (last obs == Tronco)                         = verificacao3 (Mapa l ((Rio velrio, (last obs:init obs)):t))
                                            | (elem Tronco (head (group obs))) && length (head (group obs)) > 5 = False  
                                            | otherwise                                                         = verificacao3 (Mapa l ((Rio velrio, (tail obs)):t))
verificacao3 (Mapa l ((_, obs):t)) = verificacao3 (Mapa l t)
{-|
== verificacao4
A função __'verificacao4'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 4ª condição
apresentada, "Carros têm, no máximo, 3 unidades de comprimento."

Podemos assim definir __'verificacao4'__ da seguinte maneira:

=== Definição dada
@
verificacao4 :: Mapa -> Bool
verificacao4 (Mapa l []) = True
verificacao4 (Mapa l ((Estrada _, []):t)) = verificacao4 (Mapa l t)
verificacao4 (Mapa l ((Estrada velest, obs):t)) | (elem Nenhum obs) && (last obs == Carro)                           = verificacao4 (Mapa l ((Estrada velest, (last obs: init obs)):t))
                                                | (elem Carro (head (group obs))) && (length (head (group obs)) > 3) = False
                                                | otherwise                                                          = verificacao4 (Mapa l ((Estrada velest, (tail obs)):t))
verificacao4 (Mapa l ((_, obs):t)) = verificacao4 (Mapa l t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma se o __'Mapa'__ dado contém uma 
                 lista de obstáculos com Carros de mais de 3 de comprimento
@           

Podemos reutilizar o raciocínio usado para definir a função anterior
nesta nova função, sendo os fins imensamente parecidos, só
diferindo no Terreno e no Obstáculo

== Exemplo (para uma Estrada qualquer)
>>> verificacao3 (Mapa 6 [(Estrada 3, [Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum])])
False

>>> group [Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]
[[Nenhum],[Carro,Carro,Carro,Carro],[Nenhum,Nenhum,Nenhum,Nenhum]]

>>> length [Nenhum]
1

>>> length [Carro,Carro,Carro,Carro]
4

>>> length [Nenhum,Nenhum,Nenhum,Nenhum]
4
-}
---------------------------------------------------------------------------------------------------4.
verificacao4 :: Mapa -> Bool
verificacao4 (Mapa l []) = True
verificacao4 (Mapa l ((Estrada _, []):t)) = verificacao4 (Mapa l t)
verificacao4 (Mapa l ((Estrada velest, obs):t)) | (elem Nenhum obs) && (last obs == Carro)                           = verificacao4 (Mapa l ((Estrada velest, (last obs: init obs)):t))
                                                | (elem Carro (head (group obs))) && (length (head (group obs)) > 3) = False
                                                | otherwise                                                          = verificacao4 (Mapa l ((Estrada velest, (tail obs)):t))
verificacao4 (Mapa l ((_, obs):t)) = verificacao4 (Mapa l t)
{-|
== verificacao5
A função __'verificacao5'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 5ª condição
apresentada, "Em qualquer linha existe, no mı́nimo, um “obstáculo” "__'Nenhum'__". Ou
seja, uma linha não pode ser composta exclusivamente por obstáculos,
precisando de haver pelo menos um espaço livre."

Podemos assim definir __'verificacao5'__ da seguinte maneira:

=== Definição dada
@
verificacao5 :: Mapa -> Bool
verificacao5 (Mapa larg [])           = True
verificacao5 (Mapa larg ((_, obs):t)) = (elem Nenhum obs) && verificacao5 (Mapa larg t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma se no "__'Mapa'__" dado, para todos
                 os Terrenos presentes, há uma lista de obstáculos que contem presentes 
                 pelo menos um obstáculo "__'Nenhum'__"
@           

Aqui podemos ter um raciocínio mais direto.
Para qualquer Terreno, tem de existir pelo menos 1 Obstáculo "__'Nenhum'__".
E recursivamente vemos da cabeça da lista, até ao último membro da lista
(como temos feito com a maioria das funções aqui demonstradas)

== Exemplo (para uma Estrada qualquer)
>>> verificacao5 (Mapa 6 [(Estrada 3, [Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum])])
True

>>> elem Nenhum [Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum]
True
-}
---------------------------------------------------------------------------------------------------5.
verificacao5 :: Mapa -> Bool
verificacao5 (Mapa larg [])           = True
verificacao5 (Mapa larg ((_, obs):t)) = (elem Nenhum obs) && verificacao5 (Mapa larg t)
{-|
== verificacao6
A função __'verificacao6'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 6ª condição
apresentada, "O comprimento da lista de obstáculos de cada linha corresponde exac-
tamente à largura do mapa."

Podemos assim definir __'verificacao6'__ da seguinte maneira:

=== Definição dada
@
verificacao6 :: Mapa -> Bool
verificacao6 (Mapa larg [])           = True
verificacao6 (Mapa larg ((_, obs):t)) = (length obs == larg) && verificacao6 (Mapa larg t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma que no __'Mapa'__ dado, todas
                 as listas de Obstáculos têm um comprimento igual à largura do Mapa
@           

Aqui podemos ter, mais uma vez, um raciocínio mais direto.
Se a largura da lista de Obstáculos for igual à largura do mapa,
avaliamos a próxima lista de Obstáculos do próximo par da lista de pares
até só nos restar um conjunto vazio.

== Exemplo (para uma Estrada qualquer)
>>> verificacao6 (Mapa 6 [(Estrada 3, [Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum])])
False

>>> length [Nenhum,Carro,Carro,Carro,Carro,Nenhum,Nenhum,Nenhum,Nenhum] == 6
False
-}
---------------------------------------------------------------------------------------------------6.
verificacao6 :: Mapa -> Bool
verificacao6 (Mapa larg [])           = True
verificacao6 (Mapa larg ((_, obs):t)) = (length obs == larg) && verificacao6 (Mapa larg t)


{-|
== verificacao7
A função __'verificacao7'__ irá analisar o __'Mapa'__ que lhe foi submetido pelo 
__'mapaValido'__ e dará uma avaliação binária do mesmo tendo em conta a 6ª condição
apresentada, "Contiguamente, não devem existir mais do que 4 rios, nem 5 estradas
ou relvas."

Podemos assim definir __'verificacao7'__ da seguinte maneira:

=== Definição dada
@
verificacao7 :: Mapa -> Bool
verificacao7 (Mapa larg [])                                                                                            = True
verificacao7 (Mapa larg ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t))                         = False
verificacao7 (Mapa larg ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = False
verificacao7 (Mapa larg ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t))                                    = False 
verificacao7 (Mapa larg (_:t)) = verificacao7 (Mapa larg t)
@

@
    __:: Mapa__  assume-se qualquer mapa a esta função
    __-> Bool__  a função devolve um Booleano que afirma que no __'Mapa'__ dado,
                 não existem mais de 4 __'Rios'__ ou 5 __'Estradas'__ ou __'Relvas'__ 
                 contíguos

@           

Finalmente, nesta última questão temos uma condição mais complexa,
mas para contrariar o enunciado, conseguimos simplificar isto 
de tal maneira, que só precisamos de dar 5 condições:

1. Para uma lista de pares vazia, temos que o Mapa é /True/ (Verdadeiro)

2. Para uma lista de pares, onde haja 6 pares seguidos que tenham como terreno comum __'Relvas'__,
    temos que o Mapa é /False/ (Falso)

3. Para uma lista de pares, onde haja 6 pares seguidos que tenham como terreno comum __'Estradas'__,
    temos que o Mapa é /False/ (Falso)

4. Para uma lista de pares, onde haja 5 pares seguidos que tenham como terreno comum __'Rios'__,
    temos que o Mapa é /False/ (Falso)

5. Para qualquer outro caso, analisar a lista de pares com excessão à cabeça até o conjunto
    se vir vazio (passando à primeira condição)

== Exemplo (para uma Estrada qualquer)
>>> verificacao6 (Mapa 6 [(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum])])
False

>>> verificacao6 (Mapa 6 [(Estrada 1, [Nenhum]),(Relva, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum]),(Estrada 1, [Nenhum])])
True
-}
---------------------------------------------------------------------------------------------------7.
verificacao7 :: Mapa -> Bool
verificacao7 (Mapa larg [])                                                                                            = True
verificacao7 (Mapa larg ((Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):(Relva, _):t))                         = False
verificacao7 (Mapa larg ((Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):(Estrada _, _):t)) = False
verificacao7 (Mapa larg ((Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):(Rio _, _):t))                                    = False 
verificacao7 (Mapa larg (_:t)) = verificacao7 (Mapa larg t)
