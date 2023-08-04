{- |
Module      : Tarefa6_2022li1g124
Description : Aplicação gráfica do Projeto
Copyright   : Frederico Cunha Afonso <a104001@alunos.uminho.pt>

Módulo para a realização da Tarefa 6 do projeto de LI1 em 2022/23.
-}
module Tarefa6_2022li1g124 where

import LI12223
import Tarefa1_2022li1g124
import Tarefa2_2022li1g124
import Tarefa3_2022li1g124
import Tarefa4_2022li1g124
import Tarefa5_2022li1g124
import System.Random
import Data.Char
import Graphics.Gloss
import Graphics.Gloss.Interface.Pure.Game

----------------------------------------------------------------------------------
type WORLD     = (TinyWorld, ([[Picture]],Jogo,Float,Int)) -- ^ 'WORLD' é o type que a main usará para executar o gloss, tendo este o papel de: World! Este receberá um 'TinyWorld', uma lista de listas de Pictures, usadas para desenhar o 'Jogo' na sua globalidade, Um 'Jogo' que será alterado ao longo do tempo e que será limitado pelo 'Mode' escolhido e, possívelmente, pela 'Largura' selecionada, um Float usado pela 'tokioTomare' (reageTempo) para alterar o 'Jogo' e a pontuação do Usuário apropriadamente, e um 'Int'   aleatoriamente gerado pelo programa, sempre que o ṕrograma é iniciado

type TinyWorld = (Interface,[Int],(Int,Int)) -- ^ 'TinyWorld', tal como quase tudo neste projeto, foi baseado num dos exemplos apresentados durante o decorrer das aulas, recebendo uma 'Interface' (devolvendo uma certa 'Picture'  relacionada a cada 'Interface'),uma lista de Ints que estão associadas a certas teclas pressionadas pelo Usuário, e um Tuplo de Ints que será usado mais tarde para o 'Mode' 'Classic' (Refletindo em tudo o que foi realizado até agora, era desnecessário associar um Tuplo de Ints para algo desta natureza)

{- | 'Interface' determina o que será devolvido ao Usuário, um 'Mapa' jogável com um certo 'Mode' ou um 'Menu' com vários 'SmolMenu's (pode ser visto como um 'Menu' global).
-}
data Interface
  = Menu Unlocked SmolMenu Int [Char] -- ^ o 'Menu' irá perceber se o 'Mode' 'TRUE_CLASSIC' estará disponível ou não, olhando para a lista de Chars (caso nela esteja presente __|Frogger|__ no início da lista), que também será usada para determinar a Seed do 'Jogo', e um 'Int'  para determinar que "Botão" o Usuário pode selecionar
  | Gamer Unlocked Mode Int (Bool) -- ^ o 'Gamer' recebe diferentes 'Mode's pois dependendo deste, pode devolver diferentes interpretações do mesmo mapa, um 'Int'  que está associado a uma seed gerada pelo programa, ou introduzida pelo Usuário, um Bool para detetar se o 'Jogo' acabou (usando a 'jogoTerminou') e, tal como o 'Menu', percebe se o 'Mode' 'TRUE_CLASSIC' está disponível, mas só para ter alguma consistência quando "sair" desta 'Interface' (ao pressionar 'q', o programa irá levar o Usuário da 'Interface' 'Gamer' ao 'Menu', e se este não perceber se o 'Mode' 'TRUE_CLASSIC' está disponível ou não, o Usuário terá de escrever __|Frogger|__ sempre que decidir voltar ao 'Menu') 

{- | Uma maneira mais intuitiva para o programa perceber se é possível selecionar o 'Mode' 'TRUE_CLASSIC'(ou se o Usuário já introduziu __|Frogger|__ num dado 'Menu')
-}
data Unlocked
  = Locked -- ^ 'TRUE_CLASSIC' indisponível
  | Frogger -- ^ 'TRUE_CLASSIC' disponível
 deriving (Show, Read, Eq)

{- | 'SmolMenu' pode ser visto como uma espécie de "sub-diretorias" que o 'Menu' toma, permitindo ter Projeto mais organizado.
-}
data SmolMenu
  = Queueing (Int,Int) -- ^ aqui, após ser selecionado o 'Mode' 'Classic', poderemos selecionar o tamanho do 'Mapa' que será devolvido, onde, com as setas direcionais, iremos determinar as ("Dezenas,Unidades") da 'Largura'
  | Starting -- ^ o 'SmolMenu' com que o Projeto Gráfico começa, e aquele que o jogador encontrará sempre que decidir experimentar qualquer 'Mode', ver as 'Instrucoes', usar uma Seed diferente ou simplesmente, sair apropriadamente do Projeto 
  | Instrucoes -- ^ para quem não tenha muita experiência, decidi criar esta "sub-diretoria" para apresentar todos os comandos possíveis, onde deixo uma indicação da possibilidade de desbloquear o 'Mode' 'TRUE_CLASSIC' através de letras ou usando o código ASCII para traduzir __FROGGER__
 deriving (Show, Read, Eq)

{- | Diferentes 'Mode's que serão tomadas pelo 'Gamer'.

Estes serão usados para interpretar de maneiras diferentes um 'Mapa' igual, (com uma 'Largura' variável, dependendo do 'Mode').-}
data Mode 
  = Classic Int -- ^ o 'Mode' "base" que, quando selecionado, será usado para devolver um 'Jogo' onde o 'Mapa' tem uma 'Largura' variável, decidida pelo Usuário. Para além de ser o único que permite o "deslize" de 'Obstaculos'
  | Doom -- ^ inspirado em jogos dos anos 90 (Quake, Doom, etc.), este 'Mode' permite devolver um 'Mapa' onde é possível jogar numa perspectiva em terceira pessoa, num "mundo 3D semi-realista", com uma 'Largura' fixa 7
  | TRUE_CLASSIC -- ^ o 'Mode' que é acessível após se escrever a palavra __|Frogger|__ em qualquer 'Menu'. Permite devolver uma interpretação do 'Jogo' verdadeiramente clássica, sendo o melhor 'Mode' do Projeto, com uma 'Largura' fixa de 9
  | Undefined  -- ^ um 'Mode' com que o 'Jogo' começa, cujo único propósito é ser substituído por um que não devolva um 'Mapa' "neutro" (um 'Mapa' com 1 de 'Largura', num 'Terreno' 'Relva', onde o único 'Obstaculo' presente é um 'Nenhum')
 deriving (Show, Read, Eq)


{-|

== tinyWorldInicial

A função __'tinyWorldInicial'__ é definida com um 'Menu' inicial ('Starting'), 'Locked', 
com a primeira opção selecionada (0), uma lista de Chars vazia (""), nenhum "Botão" 
selecionado ([]), e com um Tuplo de Ints nulos ((0,0)) pois não serão necessários inicialmente

Podemos assim definir __'tinyWorldInicial'__ da seguinte maneira:

=== Definição dada

@

tinyWorldInicial :: TinyWorld
tinyWorldInicial = (Menu Locked Starting 0 "",[],(0,0))

@ 

@

    __:: 'TinyWorld'__ afirma um 'TinyWorld' inicial usado posteriormente pela __'worldInicial'__

@    

-}

tinyWorldInicial :: TinyWorld
tinyWorldInicial = (Menu Locked Starting 0 "",[],(0,0))

{-|

== worldInicial

A função __'worldInicial'__ irá receber uma lista de listas de Pictures, 
um 'Jogo' e um Int atribuído aleatoriamente, todos estes são providenciados pela Main.
Assim, definindo o 'WORLD' inicial, que mais tarde será alterado dependendo do 
que o Usuário quiser fazer com o programa.
Para facilitar a definição deste 'WORLD' inicial, usei anteriormente a função 
 __'tinyWorldInicial'__, que apenas define o 'TinyWorld' inicial.


=== Definição dada

@

worldInicial:: [[Picture]] -> Jogo -> Int -> WORLD
worldInicial glossy j r = (tinyWorldInicial, (glossy,j ,0.0,r))  

@

@

    __:: [[Picture]]__  uma lista de listas de Pictures dada pela Main
    __-> 'Jogo'__   um 'Jogo' providenciado pela Main (o resultado da função
                    'jogoInicial' 0 'Undefined', um 'Jogo' com uma 'Largura' de 1,
                    'Terreno' 'Relva', e como único 'Obstaculo' um 'Nenhum')
                    Este só será utilizado até o Usuário escolher algum outro 'Mode'
    __-> Int__  um Int aleatório dado pela Main que, enquanto o Usuário decidir
                dar "seeds" vazias (""), irá progressivamente diminuindo de 1 em 1

@

Isto teve como objetivo definir um 'WORLD' inicial, que será usada no resto do trabalho,
sendo que a main (playIO) vai funcionar recursivamente até o programa ser fechado

-}

worldInicial:: [[Picture]] -> Jogo -> Int -> WORLD
worldInicial glossy j r = (tinyWorldInicial, (glossy,j ,0.0,r))  
----------------------------------------------------------------------Desenha MAPA---------------------------------------------------------------------


{-|

== worldBuilder

A função __'worldBuilder'__ irá receber 'WORLD', e dependendo da 'Interface',
irá desenhar um 'Menu', 'Jogo', etc., diferente.
Mais importante de realçar, seria o facto do 'worldBuilder' ter em atenção que:
1. o 'Unlocked', o Int do 'Menu' são relevantes no desenho de qualquer 'Menu'
2. enquanto o Usuário se encontrar nas 'Instrucoes', o 'Menu' "inicial" exposto
   é algo que interativo, pois aceita ainda seeds do Usuário, e pode até 
   deixar disponível o 'Mode' 'TRUE_CLASSIC'
3. dependendo do 'Mode' escolhido, chama recursivamente funções diferentes para
   desenhar mapas diferentes, apesar da pontuação ser sempre desenhada da mesma
   maneira.
4. ao introduzir a seed __|Frogger|__, o 'Mode' 'TRUE_CLASSIC' estará disponível
   até o Usuário fechar o programa
5. No 'Mode' 'TRUE_CLASSIC', o 'Jogo' desenha o sapo independente dos obstáculos,
   que idealmente seria como todos os outros 'Mode's deveriam fazer

=== Definição dada

@

worldBuilder :: WORLD -> Picture
worldBuilder ((Menu u Starting o seed,[_],_), (glossy,_,_,_))
    |o == 0 = scale 0.9 0.7 (Pictures [(Translate (-100) (-600) $ Color white $ Text "CLUCKER"),
                                       (Translate (90) 382.5    enter),(Translate (-835) 425    up),
                                       (Translate (-835) 375    down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 blue "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 1 = scale 0.9 0.7 (Pictures [(Translate (-100) (-600) $ Color white $ Text "DOOM"),
                                       (Translate (50) 232.5    enter),(Translate (-835) 275    up),
                                       (Translate (-835) 225    down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 blue "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 2 = scale 0.9 0.7 (Pictures [(Translate (-100) (-600) $ Color white $ Text "Cruzar Estradas?"),
                                       (Translate (-125) 82.5    enter),(Translate (-835) 125    up),
                                       (Translate (-835) 75     down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) blue "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 3 = scale 0.9 0.7 (Pictures [(Translate (-200) (-600) $ Color white $ Text "See you next time"),
                                       (Translate (-485) (-72.5)    enter),(Translate (-835) (-25)  up),
                                       (Translate (-835) (-75)  down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) blue "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 4 = scale 0.9 0.7 (Pictures [(Translate (-100) (-600) $ Color white $ Text "FROGGER"),
                                       (Translate (-255) (-222.5)    enter),(Translate (-835) (-180) up),
                                       (Translate (-835) (-225) down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) blue "Frogger")])
        where t y colour text = Translate (-800) y $ Color colour $ Text text
              up = scale 0.5 0.5 (head (last glossy))
              down = scale 0.5 0.5 ((last glossy) !! 1)
              enter = scale 0.5 0.5 ((last glossy)!!5)
              unlocked | u == Frogger = white
                       | u == Locked = black

worldBuilder (((Menu un (Queueing (d,u)) k seed),_,_), (glossy,_,_,_))
        |k == 0 = scale 0.9 0.7 (Pictures [(Translate (-100) (-600) $ Color white $ Text "CLUCKER"),
                                           (Translate 400 400 enter),horizontal,(Translate (238.5) 475 up),
                                           (Translate (238.5) 325 down),
                                           (Translate (130) (350) $ Color white $ Text (show d)), 
                                           (Translate (200) (350) $ Color blue $ Text (show u)),
                                           (t 500 white ("Seed: " ++ seed)), (t 350 blue "Classic Mode"), 
                                           (t 200 white "Doom Mode"), (t (50) white "Instrucoes"), 
                                           (t (-100) white "Sair"), (t (-250) unlocked "Frogger")])
        |k == 1 = scale 0.9 0.7 (Pictures [(Translate (-100) (-600) $ Color white $ Text "CLUCKER"),
                                           (Translate 400 400 enter),horizontal,(Translate (168.5) 475 up),
                                           (Translate (168.5) 325 down),
                                           (Translate (130) (350) $ Color blue $ Text (show d)), 
                                           (Translate (200) (350) $ Color white $ Text (show u)),
                                           (t 500 white ("Seed: " ++ seed)), (t 350 blue "Classic Mode"), 
                                           (t 200 white "Doom Mode"), (t (50) white "Instrucoes"), 
                                           (t (-100) white "Sair"), (t (-250) unlocked "Frogger")])
            where t y colour text          = Translate (-800) y $ Color colour $ Text text
                  up = scale 0.5 0.5 (head (last glossy))
                  down = scale 0.5 0.5 ((last glossy) !! 1)
                  enter = scale 0.5 0.5 ((last glossy)!!5)
                  left = scale 0.5 0.5 ((last glossy)!! 2)
                  right = scale 0.5 0.5 ((last glossy)!! 3)
                  horizontal = Pictures [(Translate 110 400 left),(Translate 297 400 right)] 
                  unlocked | un == Frogger = white
                           | un == Locked  = black

worldBuilder (((Menu un Instrucoes k seed),_,_), (glossy,_,_,_))
    |k == 0 = (Pictures [(t (-750) 310 white ("Instrucoes")), (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-110) (-850) white ("1/4"))),
                    (Translate 0 100 (scale 0.5 0.4 
                    ((Pictures [(t (-600) 300 white ("Seed: " ++ seed)), 
                                (t (-600) 150 white "Classic Mode"), (t (-600) 0 white "Doom Mode"), 
                                (t (-600) (-150) blue "Instrucoes"), (t (-600) (-300) white "Sair"), 
                                (t (-600) (-450) unlocked "Frogger")])))),
                    (Translate (-600) (-175) (img!!5)),(teclas (-550) (-15)),
                    (scale 0.3 0.5 (t (-1450) (-390) white ("|>Para navegar pelos menus, e necessario"))), 
                    (scale 0.3 0.5 (t (-1450) (-540) white ("| utilizar-se as 'setas direcionais' e o 'Enter'")))])
    |k == 1 = (Pictures [(t (-750) 310 white ("Instrucoes")), (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-120) (-850) white ("2/4"))),
                    (Translate 0 100 (scale 0.5 0.4 
                    (Pictures [(t (330) (150) white "0"),(t 400 150 blue "0"),
                                (t (-600) 300 white ("Seed: " ++ seed)), 
                                (t (-600) 150 blue "Classic Mode"), (t (-600) 0 white "Doom Mode"),
                                (t (-600) (-150) white "Instrucoes"), (t (-600) (-300) white "Sair"), 
                                (t (-600) (-450) unlocked "Frogger")]))),
                    (Translate (-600) (-175) (img!!5)),(teclas (-550) (-15)),
                    (scale 0.3 0.5 (t (-1450) (-390) white ("|>O 'Classic Mode' permite ao Jogador escolher a largura do"))), 
                    (scale 0.3 0.5 (t (-1450) (-540) white ("| mapa desde que esta seja superior a 3 e inferior a 99")))])        
    |k == 2 = (Pictures [(t (-750) 310 white ("Instrucoes")), (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-120) (-850) white ("3/4"))),
                    (scale 0.5 0.7 (t (-1565) 150 white ("Seed: 7r0663r"))),
                    (scale 0.4 0.6 (t (-775) (30) white ("XXXX"))),
                    (scale 0.4 0.6 (t (-1950) (30) white ("Seed: 70827971716982"))),
                    (scale 0.3 0.5 (t (-650) (225) white "|>Se nao for atribuida uma 'seed'")),
                    (scale 0.3 0.5 (t (-650) (70) white "| (numero qualquer usado para criar o mapa)")),
                    (scale 0.3 0.5 (t (-650) (-85) white "| ao Jogo, usando os numeros (ou o 'Delete'),")),
                    (scale 0.3 0.5 (t (-650) (-240) white "| este ira gerar uma aleatoria"))])
    |k == 3 = (Pictures [(t (-750) 310 white ("Instrucoes")), 
                    (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-122.5) (-850) white (("4/4")))),
                    (teclas (-550) (150)),
                    (scale 0.3 0.5 (t (-1450) 250 white (("|>Cada 'seta direcional' movimenta o Jogador")))), 
                    (Translate (-600) (-50) (img !! 4)),
                    (scale 0.3 0.5 (t (-1450) (-75) white "|>O 'Espaco' interrompe o Jogo")),
                    (scale 0.3 0.5 (t (-1450) (-200) white "| Qualquer tecla prossegue o Jogo")), 
                    (Translate (-650) (-200) (img !! 6)),
                    (scale 0.3 0.5 (t (-1450) (-435) white (("|>O 'Q' permite retornar ao menu anterior")))), 
                    (Translate (-550) (-300) (img !! 7)),
                    (scale 0.3 0.5 (t (-1450) (-645) white ("|>O 'R' permite reiniciar o Jogo (Jogador, Mapa e Pontuacao)")))])
        where t x y colour text = Translate x y $ Color colour $ Text text
              img = last glossy
              teclas x y = Pictures [(Translate (x-50) (y+50) (head img)),
                                     (Translate (x-50) (y-50) (img!!1)),
                                     (Translate (x+50) (y-50) (img!!3)),
                                     (Translate (x-150) (y-50) (img!!2))]
              unlocked | un == Frogger = white
                       | un == Locked = black

worldBuilder ((Gamer _ gamemode seed gameBool,_,(xo,yo)), (glossy,(Jogo (Jogador (x, y)) (Mapa larg terobs)),n,r))
    | gamemode == TRUE_CLASSIC                                   
        = pictures (((froggerTerrenos (glossy !! 5) (map fst terobs) ((-337.5),(412.5))) 
                     ++ (froggerObstaculos (glossy !! 5) (terobs) ((-337.5),(412.5)))  
                     ++ [Translate (-337.5+((fromIntegral x)*75)) (412.5-((fromIntegral y)*75)) ((glossy !! 5) !! 9)]) 
                     ++ (scoreboard gameBool n))
    | gamemode == Doom                                           
        = pictures ((terrenosDoom ((glossy!!2),(glossy!!3),(glossy!!4),(glossy!!0)) (Mapa larg terobs) (x,y) (0,0))
                    ++(scoreboard gameBool n))
    | gamemode /= Doom && gamemode /= TRUE_CLASSIC && larg < 19  
        = (pictures ((terrenosClassic (glossy!!1) (map fst terobs) ((-45*((fromIntegral larg)-1)),
                     (45*((fromIntegral (alturaMapa larg))-1))) larg) 
                    ++ (obstaculosClassic (head glossy) (Mapa larg terobs) n ((-45*((fromIntegral larg)-1)),
                        (45*((fromIntegral (alturaMapa larg))-1))) (xo,yo))
                    ++(scoreboard gameBool n)))
    | gamemode /= Doom && gamemode /= TRUE_CLASSIC && larg >= 19 
        = (pictures (scale 0.9875 0.9875  (scale (18/ (fromIntegral larg)) (18/ (fromIntegral larg)) 
            (pictures 
                ((terrenosClassic (glossy!!1) (map fst terobs) ((-45*((fromIntegral larg)-1)),
                    (45*((fromIntegral (alturaMapa larg))-1))) larg) 
                ++ (obstaculosClassic (head glossy) (Mapa larg terobs) n ((-45*((fromIntegral larg)-1)),
                    (45*((fromIntegral (alturaMapa larg))-1))) (xo,yo))))):(scoreboard gameBool n)))

@

@

    __:: 'WORLD'__  wm 'WORLD' qualquer que impõe o que deverá ser desenhado 

    __-> Picture__   devolve uma imagem final, imposta pelo 'World'
@

-}

worldBuilder :: WORLD -> Picture
worldBuilder ((Menu u Starting o seed,[_],_), (glossy,_,_,_))
    |o == 0 = scale 1.08 0.84 (Pictures [(Translate (285) (-600) $ Color white $ Text "CLUCKER"),
                                       (Translate (90) 382.5    enter),(Translate (-835) 425    up),
                                       (Translate (-835) 375    down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 blue "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 1 = scale 1.08 0.84 (Pictures [(Translate (500) (-600) $ Color white $ Text "DOOM"),
                                       (Translate (50) 232.5    enter),(Translate (-835) 275    up),
                                       (Translate (-835) 225    down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 blue "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 2 = scale 1.08 0.84 (Pictures [(Translate (-200) (-600) $ Color white $ Text "Cruzar Estradas?"),
                                       (Translate (-125) 82.5    enter),(Translate (-835) 125    up),
                                       (Translate (-835) 75     down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) blue "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 3 = scale 1.08 0.84 (Pictures [(Translate (-375) (-600) $ Color white $ Text "See you next time"),
                                       (Translate (-485) (-72.5)    enter),(Translate (-835) (-25)  up),
                                       (Translate (-835) (-75)  down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) blue "Sair"), 
                                       (t (-250) unlocked "Frogger")])
    |o == 4 = scale 1.08 0.84 (Pictures [(Translate (264) (-600) $ Color white $ Text "FROGGER"),
                                       (Translate (-255) (-222.5)    enter),(Translate (-835) (-180) up),
                                       (Translate (-835) (-225) down),(t 500 white ("Seed: " ++ seed)), 
                                       (t 350 white "Classic Mode"), (t 200 white "Doom Mode"), 
                                       (t (50) white "Instrucoes"), (t (-100) white "Sair"), 
                                       (t (-250) blue "Frogger")])
        where t y colour text = Translate (-800) y $ Color colour $ Text text
              up = scale 0.5 0.5 (head (last glossy))
              down = scale 0.5 0.5 ((last glossy) !! 1)
              enter = scale 0.5 0.5 ((last glossy)!!5)
              unlocked | u == Frogger = white
                       | u == Locked = black

worldBuilder (((Menu un (Queueing (d,u)) k seed),_,_), (glossy,_,_,_))
        |k == 0 = scale 1.08 0.84 (Pictures [(Translate (-350) (-600) $ Color white $ Text "CLUCKER"),
                                           (Translate 400 400 enter),horizontal,(Translate (238.5) 475 up),
                                           (Translate (238.5) 325 down),
                                           (Translate (130) (350) $ Color white $ Text (show d)), 
                                           (Translate (200) (350) $ Color blue $ Text (show u)),
                                           (t 500 white ("Seed: " ++ seed)), (t 350 blue "Classic Mode"), 
                                           (t 200 white "Doom Mode"), (t (50) white "Instrucoes"), 
                                           (t (-100) white "Sair"), (t (-250) unlocked "Frogger")])
        |k == 1 = scale 1.08 0.84 (Pictures [(Translate (-350) (-600) $ Color white $ Text "CLUCKER"),
                                           (Translate 400 400 enter),horizontal,(Translate (168.5) 475 up),
                                           (Translate (168.5) 325 down),
                                           (Translate (130) (350) $ Color blue $ Text (show d)), 
                                           (Translate (200) (350) $ Color white $ Text (show u)),
                                           (t 500 white ("Seed: " ++ seed)), (t 350 blue "Classic Mode"), 
                                           (t 200 white "Doom Mode"), (t (50) white "Instrucoes"), 
                                           (t (-100) white "Sair"), (t (-250) unlocked "Frogger")])
            where t y colour text          = Translate (-800) y $ Color colour $ Text text
                  up = scale 0.5 0.5 (head (last glossy))
                  down = scale 0.5 0.5 ((last glossy) !! 1)
                  enter = scale 0.5 0.5 ((last glossy)!!5)
                  left = scale 0.5 0.5 ((last glossy)!! 2)
                  right = scale 0.5 0.5 ((last glossy)!! 3)
                  horizontal = Pictures [(Translate 110 400 left),(Translate 297 400 right)] 
                  unlocked | un == Frogger = white
                           | un == Locked  = black

worldBuilder (((Menu un Instrucoes k seed),_,_), (glossy,_,_,_))
    |k == 0 = scale 1.2 1.2(Pictures [(t (-750) 310 white ("Instrucoes")), (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-110) (-850) white ("1/4"))),
                    (Translate 0 100 (scale 0.5 0.4 
                    ((Pictures [(t (-600) 300 white ("Seed: " ++ seed)), 
                                (t (-600) 150 white "Classic Mode"), (t (-600) 0 white "Doom Mode"), 
                                (t (-600) (-150) blue "Instrucoes"), (t (-600) (-300) white "Sair"), 
                                (t (-600) (-450) unlocked "Frogger")])))),
                    (Translate (-600) (-175) (img!!5)),(teclas (-550) (-15)),
                    (scale 0.3 0.5 (t (-1450) (-390) white ("|>Para navegar pelos menus, e necessario"))), 
                    (scale 0.3 0.5 (t (-1450) (-540) white ("| utilizar-se as 'setas direcionais' e o 'Enter'")))])
    |k == 1 = scale 1.2 1.2(Pictures [(t (-750) 310 white ("Instrucoes")), (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-120) (-850) white ("2/4"))),
                    (Translate 0 100 (scale 0.5 0.4 
                    (Pictures [(t (330) (150) white "0"),(t 400 150 blue "0"),
                                (t (-600) 300 white ("Seed: " ++ seed)), 
                                (t (-600) 150 blue "Classic Mode"), (t (-600) 0 white "Doom Mode"),
                                (t (-600) (-150) white "Instrucoes"), (t (-600) (-300) white "Sair"), 
                                (t (-600) (-450) unlocked "Frogger")]))),
                    (Translate (-600) (-175) (img!!5)),(teclas (-550) (-15)),
                    (scale 0.3 0.5 (t (-1450) (-390) white ("|>O 'Classic Mode' permite ao Jogador escolher a largura do"))), 
                    (scale 0.3 0.5 (t (-1450) (-540) white ("| mapa desde que esta seja superior a 3 e inferior a 99")))])        
    |k == 2 = scale 1.2 1.2(Pictures [(t (-750) 310 white ("Instrucoes")), (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-120) (-850) white ("3/4"))),
                    (scale 0.5 0.7 (t (-1565) 150 white ("Seed: 7r0663r"))),
                    (scale 0.4 0.6 (t (-775) (30) white ("XXXX"))),
                    (scale 0.4 0.6 (t (-1950) (30) white ("Seed: 70827971716982"))),
                    (scale 0.3 0.5 (t (-650) (225) white "|>Se nao for atribuida uma 'seed'")),
                    (scale 0.3 0.5 (t (-650) (70) white "| (numero qualquer usado para criar o mapa)")),
                    (scale 0.3 0.5 (t (-650) (-85) white "| ao Jogo, usando os numeros (ou o 'Delete'),")),
                    (scale 0.3 0.5 (t (-650) (-240) white "| este ira gerar uma aleatoria"))])
    |k == 3 = scale 1.2 1.2(Pictures [(t (-750) 310 white ("Instrucoes")), 
                    (Translate 100 (-400) (scale 0.8 0.8(img!!3))), 
                    (Translate (-100) (-400) (scale 0.8 0.8(img!!2))),
                    (scale 0.3 0.5 (t (-122.5) (-850) white (("4/4")))),
                    (teclas (-550) (150)),
                    (scale 0.3 0.5 (t (-1450) 250 white (("|>Cada 'seta direcional' movimenta o Jogador")))), 
                    (Translate (-600) (-50) (img !! 4)),
                    (scale 0.3 0.5 (t (-1450) (-75) white "|>O 'Espaco' interrompe o Jogo")),
                    (scale 0.3 0.5 (t (-1450) (-200) white "| Qualquer tecla prossegue o Jogo")), 
                    (Translate (-650) (-200) (img !! 6)),
                    (scale 0.3 0.5 (t (-1450) (-435) white (("|>O 'Q' permite retornar ao menu anterior")))), 
                    (Translate (-550) (-300) (img !! 7)),
                    (scale 0.3 0.5 (t (-1450) (-645) white ("|>O 'R' permite reiniciar o Jogo (Jogador, Mapa e Pontuacao)")))])
        where t x y colour text = Translate x y $ Color colour $ Text text
              img = last glossy
              teclas x y = Pictures [(Translate (x-50) (y+50) (head img)),
                                     (Translate (x-50) (y-50) (img!!1)),
                                     (Translate (x+50) (y-50) (img!!3)),
                                     (Translate (x-150) (y-50) (img!!2))]
              unlocked | un == Frogger = white
                       | un == Locked = black

worldBuilder ((Gamer _ gamemode seed gameBool,_,(xo,yo)), (glossy,(Jogo (Jogador (x, y)) (Mapa larg terobs)),n,r))
    | gamemode == TRUE_CLASSIC                                   
        = scale 1.2 1.2 (pictures (((froggerTerrenos (glossy !! 5) (map fst terobs) ((-337.5),(412.5))) 
                                    ++ (froggerObstaculos (glossy !! 5) (terobs) ((-337.5),(412.5)))  
                                    ++ [Translate (-337.5+((fromIntegral x)*75)) (412.5-((fromIntegral y)*75)) ((glossy !! 5) !! 9)]) 
                                    ++ (scoreboard gameBool n)))
    | gamemode == Doom                                           
        = scale 1.2 1.2 (Translate 0 (-60) (pictures ((terrenosDoom ((glossy!!2),(glossy!!3),(glossy!!4),(glossy!!0)) (Mapa larg terobs) (x,y) (0,0))
                    ++(scoreboard gameBool n))))
    | gamemode /= Doom && gamemode /= TRUE_CLASSIC && larg < 19  
        = scale 1.2 1.2 (pictures ((terrenosClassic (glossy!!1) (map fst terobs) ((-45*((fromIntegral larg)-1)),
                     (45*((fromIntegral (alturaMapa larg))-1))) larg) 
                    ++ (obstaculosClassic (head glossy) (Mapa larg terobs) n ((-45*((fromIntegral larg)-1)),
                        (45*((fromIntegral (alturaMapa larg))-1))) (xo,yo))
                    ++(scoreboard gameBool n)))
    | gamemode /= Doom && gamemode /= TRUE_CLASSIC && larg >= 19 
        = scale 1.2 1.2 (pictures (scale 0.9875 0.9875  (scale (18/ (fromIntegral larg)) (18/ (fromIntegral larg)) 
            (pictures 
                ((terrenosClassic (glossy!!1) (map fst terobs) ((-45*((fromIntegral larg)-1)),
                    (45*((fromIntegral (alturaMapa larg))-1))) larg) 
                ++ (obstaculosClassic (head glossy) (Mapa larg terobs) n ((-45*((fromIntegral larg)-1)),
                    (45*((fromIntegral (alturaMapa larg))-1))) (xo,yo))))):(scoreboard gameBool n)))
----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
{-|

== scoreboard

A função __'scoreboard'__ irá receber um Bool e um Float
tal que, quando o Bool se tornar num "True", a Picture desenhada
"aproxima-se do ecrã" e mostra uma mensagem, dependendo do Float
recebido.
Caso se mantenha "False", irá mostrar a pontuação atual do Usuário.  


=== Definição dada

@

scoreboard :: Bool -> Float -> [Picture]
scoreboard gameBool n | gameBool == False =    [Translate (-620) 375 (Color (colour) $ rectangleSolid 320 120)] 
                                            ++ [Translate (-620) 375 (Color (black) $ rectangleSolid 310 110)]
                                            ++ [Translate (-775) 324 (Color (colour) $ Text (show $(div (round (n)) 600)))]
                      | otherwise         =    [Translate (-37.5) 200 (Color (colour) $ rectangleSolid 760 240)] 
                                            ++ [Translate (-37.5) 200 (Color (black) $ rectangleSolid 750 230)] 
                                            ++ [Translate (-37.5) 200 (Color (black) $ rectangleSolid 750 230)]
                                            ++ [scale 0.6 0.7 (Translate (-675) 315 (Color (colour) $ Text gameover))]
                                            ++ [Translate (-407.5) 95 (Color (colour) $ Text ("Score " 
                                            ++ (show $(div (round (n)) 600))))]
                        where (gameover, colour) = detectGameDifficulty n
                              detectGameDifficulty n | n < 12000                 = ("TentaDeNovo! (<20)",white)
                                                     | n >= 12000 && n < 60000   = ("Boa! (<100)",green)
                                                     | n >= 60000 && n < 120000  = ("MuitoBem! (<200)",yellow)
                                                     | n >= 120000 && n < 240000 = ("MESTRE|DO|DOMINIO!",orange)
                                                     | n >= 240000               = ("Estrada Atravessada",red)

@

@

    __:: Bool__  um Bool que determina se o 'Jogo' acabou
    __-> Float__  um Float que funciona para determinar a pontuação
                  do Usuário, do início ao fim do 'Jogo'
    __-> [Picture]__  devolve uma lista de Pictures que mais tarde
                      serão processadas pela __'worldBuilder'__ 

@

Enquanto o 'Jogo' não acabar, a cor da imagem mostrada irá mudar progressivamente, até estar Vermelha. 
-}
scoreboard :: Bool -> Float -> [Picture]
scoreboard gameBool n | gameBool == False =    [Translate (-620) 375 (Color (colour) $ rectangleSolid 320 120)] 
                                            ++ [Translate (-620) 375 (Color (black) $ rectangleSolid 310 110)]
                                            ++ [Translate (-775) 324 (Color (colour) $ Text (show $(div (round (n)) 600)))]
                      | otherwise         =    [Translate (-37.5) 200 (Color (colour) $ rectangleSolid 760 240)] 
                                            ++ [Translate (-37.5) 200 (Color (black) $ rectangleSolid 750 230)] 
                                            ++ [Translate (-37.5) 200 (Color (black) $ rectangleSolid 750 230)]
                                            ++ [scale 0.6 0.7 (Translate (-675) 315 (Color (colour) $ Text gameover))]
                                            ++ [Translate (-407.5) 95 (Color (colour) $ Text ("Score " 
                                            ++ (show $(div (round (n)) 600))))]
                        where (gameover, colour) = detectGameDifficulty n
                              detectGameDifficulty n | n < 12000                 = ("TentaDeNovo! (<20)",white)
                                                     | n >= 12000 && n < 60000   = ("Boa! (<100)",green)
                                                     | n >= 60000 && n < 120000  = ("MuitoBem! (<200)",yellow)
                                                     | n >= 120000 && n < 240000 = ("MESTRE|DO|DOMINIO!",orange)
                                                     | n >= 240000               = ("Estrada Atravessada",red)
---------------------------------------------------------------Frogger-------------------------------------------------------------------------------------------------------------
{-|

== froggerTerrenos

A função __'froggerTerrenos'__ irá avaliar a lista de 'Terreno's dada e irá desenhá-los linha a linha.
Começa por avaliar a cabeça da lista e, dependendo da cabeça, desenha sempre a mesma imagem, de 75 em 
75 píxeis até ter uma linha completa do mesmo desenho (desenhando assim o 'Terreno'). Quando tal 
acontecer, desce 75 píxeis e vai para a abcissa inicial e repete o processo.

=== Definição dada

@

froggerTerrenos :: [Picture] -> [Terreno] -> (Float,Float) -> [Picture]
froggerTerrenos _ [] _                       = []
froggerTerrenos images (h:t) (337.5,y)       = froggerTerrenos images t ((-337.5),(y-75))
froggerTerrenos images (Relva:t) (x,y)       = (Translate x y (images !! 11)):(froggerTerrenos images (Relva:t) ((x+75),y))
froggerTerrenos images ((Estrada k):t) (_,y) = froggerTerrenos images t ((-337.5),(y-75))
froggerTerrenos images ((Rio k):t) (x,y)     = (Translate x y river):(froggerTerrenos images ((Rio k):t) ((x+75),y))
    where river = color (dark blue) $ rectangleSolid 75 75 

@

@

    __:: [Picture]__  uma lista de pictures dada pela __'worldBuilder'__
    __-> ['Terreno']__  uma lista de terrenos que a função analisa
                      recursivamente
    __-> (Float,Float)__  um tuplo de Floats que indica onde a imagem
                          deve estar
    __-> [Picture]__  devolve uma lista de Pictures

@
-}
froggerTerrenos :: [Picture] -> [Terreno] -> (Float,Float) -> [Picture]
froggerTerrenos _ [] _                       = []
froggerTerrenos images (h:t) (337.5,y)       = froggerTerrenos images t ((-337.5),(y-75))
froggerTerrenos images (Relva:t) (x,y)       = (Translate x y (images !! 11)):(froggerTerrenos images (Relva:t) ((x+75),y))
froggerTerrenos images ((Estrada k):t) (_,y) = froggerTerrenos images t ((-337.5),(y-75))
froggerTerrenos images ((Rio k):t) (x,y)     = (Translate x y river):(froggerTerrenos images ((Rio k):t) ((x+75),y))
    where river = color (dark blue) $ rectangleSolid 75 75    

{-|

== froggerObstaculos

A função __'froggerObstaculos'__ irá avaliar a lista de 'Obstaculo's dada e irá desenhá-los de
75 em 75 píxeis. Tomando em atenção que, querendo que houvessem diferentes veículos na 'Estrada',
tinha de ter em atenção as diferentes posições em que a variável 'Carro' se podia encontrar, e que,
querendo que os troncos fossem "conectados", a função tem a noção das diferentes posições que este
podia ter

=== Definição dada

@

froggerObstaculos :: [Picture] -> [(Terreno,[Obstaculo])] -> (Float,Float) -> [Picture]
froggerObstaculos _ [] _ = []
froggerObstaculos images ((_,[]):t) (_,y) = froggerObstaculos images t ((-337.5),(y-75))
froggerObstaculos images ((ter,(h:t)):lt) (x,y)
    | h == Arvore                                                           = (Translate x y (images !! 10)) : (froggerObstaculos images ((ter,t):lt) ((x+75),y))
    | length (h:t) == 9 && (h: (drop 6 t)) == [Carro,Carro,Carro]           = (Translate x y (guiar(images !! (1+(signum(velTer1 ter)))))):(froggerObstaculos images ((ter,(init(init t))):lt) ((x+75),y))++[Translate (187.5) y (guiar(images !! (1-(signum(velTer1 ter)))))]++[(Translate (262.5) y (guiar(images !! 1)))]     
    | length (h:t) == 9 && ((last t):(take 2 (h:t))) == [Carro,Carro,Carro] = (Translate x y (guiar(images !! 1))):(Translate (x+75) y (guiar(images !! (1+(signum(velTer1 ter)))))):(froggerObstaculos images ((ter,(init(tail t))):lt) ((x+150),y))++[(Translate (262.5) y (guiar(images !! (1-(signum(velTer1 ter))))))]
    | length (h:t) == 9 && (h:[last t]) == [Carro,Carro]                    = (Translate x y (guiar(images !! ((div (9-(signum(velTer1 ter))) 2))))):(froggerObstaculos images ((ter,(init t)):lt) ((x+75),y))++[(Translate (262.5) y (guiar(images !! (div (9+(signum(velTer1 ter))) 2))))]
    | take 2 (h:t) == [Tronco,Nenhum]                                       = (Translate (x) y (images !! 8): (froggerObstaculos images ((ter,(t)):lt) (x+75,y)))
    | take 2 (h:t) == [Nenhum,Tronco]                                       = (Translate (x+75) y (images !! 6): (froggerObstaculos images ((ter,(tail t)):lt) (x+150,y)))
    | h == Tronco                                                           = (Translate x y (images !! 7): (froggerObstaculos images ((ter,t):lt) (x+75,y)))
    | take 3 (h:t) == [Carro,Carro,Carro]                                   = (Translate x y (guiar(images !! (1-(signum(velTer1 ter)))))):(Translate (x+75) y (guiar(images !! 1))):(Translate (x+150) y (guiar(images !! (1+(signum(velTer1 ter)))))) : (froggerObstaculos images ((ter,(tail(tail t))):lt) ((x+225),y))
    | take 2 (h:t) == [Carro,Carro]                                         = (Translate x y (guiar(images !! (div (9+(signum(velTer1 ter))) 2)))):(Translate (x+75) y (guiar(images !! (div (9-(signum(velTer1 ter))) 2)))):(froggerObstaculos images ((ter,(tail t)):lt) ((x+150),y))
    | h == Carro                                                            = (Translate x y (guiar(images !! 3))):(froggerObstaculos images ((ter,t):lt) ((x+75),y))
    | otherwise                                                             = froggerObstaculos images ((ter,t):lt) ((x+75),y)
        where guiar = rotate (90-(fromIntegral(signum (velTer1 ter))*90))

@

@

    __:: [Picture]__  uma lista de pictures dada pela __'worldBuilder'__
    __-> [('Terreno',['Obstaculo'])]__  uma lista de tuplos, ('Terreno',['Obstaculo']) 
                                    que a função analisa recursivamente
    __-> (Float,Float)__  um tuplo de Floats que indica onde a imagem
                          deve estar
    __-> [Picture]__  devolve uma lista de Pictures

@

-}

froggerObstaculos :: [Picture] -> [(Terreno,[Obstaculo])] -> (Float,Float) -> [Picture]
froggerObstaculos _ [] _ = []
froggerObstaculos images ((_,[]):t) (_,y) = froggerObstaculos images t ((-337.5),(y-75))
froggerObstaculos images ((ter,(h:t)):lt) (x,y)
    | h == Arvore                                                           = (Translate x y (images !! 10)) : (froggerObstaculos images ((ter,t):lt) ((x+75),y))
    | length (h:t) == 9 && (h: (drop 6 t)) == [Carro,Carro,Carro]           = (Translate x y (guiar(images !! (1+(signum(velTer1 ter)))))):(froggerObstaculos images ((ter,(init(init t))):lt) ((x+75),y))++[Translate (187.5) y (guiar(images !! (1-(signum(velTer1 ter)))))]++[(Translate (262.5) y (guiar(images !! 1)))]     
    | length (h:t) == 9 && ((last t):(take 2 (h:t))) == [Carro,Carro,Carro] = (Translate x y (guiar(images !! 1))):(Translate (x+75) y (guiar(images !! (1+(signum(velTer1 ter)))))):(froggerObstaculos images ((ter,(init(tail t))):lt) ((x+150),y))++[(Translate (262.5) y (guiar(images !! (1-(signum(velTer1 ter))))))]
    | length (h:t) == 9 && (h:[last t]) == [Carro,Carro]                    = (Translate x y (guiar(images !! ((div (9-(signum(velTer1 ter))) 2))))):(froggerObstaculos images ((ter,(init t)):lt) ((x+75),y))++[(Translate (262.5) y (guiar(images !! (div (9+(signum(velTer1 ter))) 2))))]
    | take 2 (h:t) == [Tronco,Nenhum]                                       = (Translate (x) y (images !! 8): (froggerObstaculos images ((ter,(t)):lt) (x+75,y)))
    | take 2 (h:t) == [Nenhum,Tronco]                                       = (Translate (x+75) y (images !! 6): (froggerObstaculos images ((ter,(tail t)):lt) (x+150,y)))
    | h == Tronco                                                           = (Translate x y (images !! 7): (froggerObstaculos images ((ter,t):lt) (x+75,y)))
    | take 3 (h:t) == [Carro,Carro,Carro]                                   = (Translate x y (guiar(images !! (1-(signum(velTer1 ter)))))):(Translate (x+75) y (guiar(images !! 1))):(Translate (x+150) y (guiar(images !! (1+(signum(velTer1 ter)))))) : (froggerObstaculos images ((ter,(tail(tail t))):lt) ((x+225),y))
    | take 2 (h:t) == [Carro,Carro]                                         = (Translate x y (guiar(images !! (div (9+(signum(velTer1 ter))) 2)))):(Translate (x+75) y (guiar(images !! (div (9-(signum(velTer1 ter))) 2)))):(froggerObstaculos images ((ter,(tail t)):lt) ((x+150),y))
    | h == Carro                                                            = (Translate x y (guiar(images !! 3))):(froggerObstaculos images ((ter,t):lt) ((x+75),y))
    | otherwise                                                             = froggerObstaculos images ((ter,t):lt) ((x+75),y)
        where guiar = rotate (90-(fromIntegral(signum (velTer1 ter))*90))
    
{-|

== terrenosClassic

A função __'terrenosClassic'__ irá avaliar a lista de 'Terreno's dada e irá desenhá-los linha a linha.
Começa por avaliar a cabeça da lista e, dependendo da cabeça, desenha sempre a mesma imagem, de 90 em 
90 píxeis até ter uma linha completa do mesmo desenho (desenhando assim o 'Terreno'). Quando tal 
acontecer, desce 90 píxeis e vai para a abcissa inicial e repete o processo.

=== Definição dada

@

terrenosClassic :: [Picture] -> [Terreno] -> (Float,Float) -> Largura -> [Picture]
terrenosClassic _ [] _ _                              = [Blank]
terrenosClassic terrenos (h:t) (x,y) larg             | x == ((-45*(-(fromIntegral larg)-1))) = terrenosClassic terrenos t ((x-(90*(fromIntegral larg))),(y-90)) larg
terrenosClassic terrenos (Relva:t) (x,y) larg         = (Translate x y (terrenos !! 0)):(terrenosClassic terrenos (Relva:t) (x+90,y) larg) 
terrenosClassic terrenos ((Rio vel):t) (x,y) larg     = (Translate x y (terrenos !! 1)):(terrenosClassic terrenos ((Rio vel):t) ((x+90),y) larg) 
terrenosClassic terrenos ((Estrada vel):t) (x,y) larg = (Translate x y (terrenos !! 2)):(terrenosClassic terrenos ((Estrada vel):t) (x+90,y) larg) 

@

@

    __:: ['Picture']__  uma lista de pictures dada pela __'worldBuilder'__
    __-> ['Terreno']__  uma lista de terrenos que a função analisa
                      recursivamente
    __-> (Float,Float)__  um tuplo de Floats que indica onde a imagem
                          deve estar
    __-> 'Largura'__  recebe uma 'Largura' porque permiti que neste 'Mode'
                      a 'Largura' fosse variável, o que não acontece com
                      mais nenhum 'Mode'
    __-> [Picture]__  devolve uma lista de Pictures

@

-}
---------------------------------------------------------------CLASSIC GLOSS SIMPLE-------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------
terrenosClassic :: [Picture] -> [Terreno] -> (Float,Float) -> Largura -> [Picture]
terrenosClassic _ [] _ _                              = [Blank]
terrenosClassic terrenos (h:t) (x,y) larg             | x == ((-45*(-(fromIntegral larg)-1))) = terrenosClassic terrenos t ((x-(90*(fromIntegral larg))),(y-90)) larg
terrenosClassic terrenos (Relva:t) (x,y) larg         = (Translate x y (terrenos !! 0)):(terrenosClassic terrenos (Relva:t) (x+90,y) larg) 
terrenosClassic terrenos ((Rio vel):t) (x,y) larg     = (Translate x y (terrenos !! 1)):(terrenosClassic terrenos ((Rio vel):t) ((x+90),y) larg) 
terrenosClassic terrenos ((Estrada vel):t) (x,y) larg = (Translate x y (terrenos !! 2)):(terrenosClassic terrenos ((Estrada vel):t) (x+90,y) larg) 

{-|

== obstaculosClassic

A função __'obstaculosClassic'__ irá avaliar o 'Mapa' dado e irá desenhar os 'Obstaculo's presentes de
90 em 90 píxeis. Querendo que este 'Mode' tivesse um movimento de "deslize suave", foi necessário
implementar uma função à parte:  __deslocaObs__.
=== Definição dada

@

obstaculosClassic :: [Picture] -> Mapa -> Float -> (Float,Float) -> Coordenadas ->  [Picture]
obstaculosClassic obstaculos (Mapa larg []) _ (x,y) (xo,yo) 
    = [Translate (fromIntegral xo) (fromIntegral yo) (scale (1/3.5) (1/3.5) (obstaculos !! 2))]
obstaculosClassic obstaculos (Mapa larg ((_,[]):t)) n (x,y) (xo,yo)
    = obstaculosClassic obstaculos (Mapa larg t) n ((x-(90*(fromIntegral larg))),(y-90)) (xo,yo)
obstaculosClassic obstaculos (Mapa larg ((h,(l:lt)):t)) n (x,y) (xo,yo)
    | take 2 (l:lt) == [Tronco,Nenhum]                    = (Translate (x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 6))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Tronco                                         = (Translate (30+x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 1))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Tronco && xo == (round x) && (round y) == yo   = (Translate (30+x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 1))):(Translate (fromIntegral xo+(deslocaObs vel (n-1))) (fromIntegral yo) (scale (1/3.5) (1/3.5) (obstaculos !! 2))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (200000+xo,0)
    | l == Tronco && (xo /= (round x) || (round y) /= yo) = (Translate (30+x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 1))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Arvore                                         = (Translate x (y-5) (scale 0.335 0.285 (obstaculos !! 0))):(obstaculosClassic obstaculos (Mapa larg ((Relva, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Carro                                          = (Translate (x+(deslocaObs vel (n-1))) y (scale (fromIntegral(signum vel)*(1/2.75)) (1/2.75) (obstaculos !! 3))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | otherwise                                           = (obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
                where (novoX,novoY) | h == (Rio (velTer1 h)) && xo == (round x) && (round y) == yo = (xo+(round(deslocaObs (velTer1 h) (n))),yo)
                                    | otherwise                                                  = (xo,yo)
                      vel = velTer1 h
                      deslocaObs vel n | abs vel == 3 = ((((fromIntegral vel)) * (fromIntegral(mod ((round n)) 200)))/100*5*(abs (fromIntegral vel))) + (fromIntegral(-45 * signum vel))
                                       | abs vel == 2 = ((((fromIntegral vel)) * (fromIntegral(mod ((round n)) 400)))/100*5.5*(abs (fromIntegral vel))) + (fromIntegral(-45 * signum vel))
                                       | abs vel == 1 = ((((fromIntegral vel)) * (fromIntegral(mod ((round n)) 600)))/100*15*(abs (fromIntegral vel))) + (fromIntegral(-45 * signum vel))

@

@

    __:: [Picture]__  uma lista de pictures dada pela __'worldBuilder'__
    __-> 'Mapa'__  um 'Mapa' que a função analisa recursivamente
    __-> Float__  um Float que a função usa para aplicar o "deslize suave"
                  mencionado anteriormente
    __-> (Float,Float)__  um tuplo de Floats que indica onde a imagem
                          deve estar
    __-> 'Coordenadas'__  um tuplo usado para desenhar o 'Jogador' no 'Mapa'
    __-> [Picture]__  devolve uma lista de Pictures

@

-}

obstaculosClassic :: [Picture] -> Mapa -> Float -> (Float,Float) -> Coordenadas ->  [Picture]
obstaculosClassic obstaculos (Mapa larg []) _ (x,y) (xo,yo) 
    = [Translate (fromIntegral xo) (fromIntegral yo) (scale (1/3.5) (1/3.5) (obstaculos !! 2))]
obstaculosClassic obstaculos (Mapa larg ((_,[]):t)) n (x,y) (xo,yo)
    = obstaculosClassic obstaculos (Mapa larg t) n ((x-(90*(fromIntegral larg))),(y-90)) (xo,yo)
obstaculosClassic obstaculos (Mapa larg ((h,(l:lt)):t)) n (x,y) (xo,yo)
    | take 2 (l:lt) == [Tronco,Nenhum]                    = (Translate (x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 6))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Tronco                                         = (Translate (30+x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 1))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Tronco && xo == (round x) && (round y) == yo   = (Translate (30+x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 1))):(Translate (fromIntegral xo+(deslocaObs vel (n-1))) (fromIntegral yo) (scale (1/3.5) (1/3.5) (obstaculos !! 2))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (200000+xo,0)
    | l == Tronco && (xo /= (round x) || (round y) /= yo) = (Translate (30+x+(deslocaObs vel (n-1))) (y+30) (scale (-1/2) (1/2) (obstaculos !! 1))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Arvore                                         = (Translate x (y-5) (scale 0.335 0.285 (obstaculos !! 0))):(obstaculosClassic obstaculos (Mapa larg ((Relva, lt):t)) n (x+90,y)) (novoX,novoY)
    | l == Carro                                          = (Translate (x+(deslocaObs vel (n-1))) y (scale (fromIntegral(signum vel)*(1/2.75)) (1/2.75) (obstaculos !! 3))):(obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
    | otherwise                                           = (obstaculosClassic obstaculos (Mapa larg ((h, lt):t)) n (x+90,y)) (novoX,novoY)
                where (novoX,novoY) | h == (Rio (velTer1 h)) && xo == (round x) && (round y) == yo = (xo+(round(deslocaObs (velTer1 h) (n))),yo)
                                    | otherwise                                                  = (xo,yo)
                      vel = velTer1 h
                      deslocaObs vel n | abs vel == 3 = ((((fromIntegral vel)) * (fromIntegral(mod ((round n)) 200)))/100*5*(abs (fromIntegral vel))) + (fromIntegral(-45 * signum vel))
                                       | abs vel == 2 = ((((fromIntegral vel)) * (fromIntegral(mod ((round n)) 400)))/100*5.5*(abs (fromIntegral vel))) + (fromIntegral(-45 * signum vel))
                                       | abs vel == 1 = ((((fromIntegral vel)) * (fromIntegral(mod ((round n)) 600)))/100*15*(abs (fromIntegral vel))) + (fromIntegral(-45 * signum vel))



-----------------------------------------------------------------GLOSS 3D COMPLICADO-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|

== terrenosDoom

A função __'terrenosDoom'__ irá avaliar o 'Mapa' dado e irá desenhar os 'Terreno's presentes linha a linha.
Começa por avaliar a cabeça da lista e, dependendo da cabeça e da posição do 'Terreno' na lista de tuplos,
desenha a imagem do 'Terreno' e posição equivalente. Com auxílio da __'worldBuilder'__, o 'Mapa' deste
'Mode' é desenhado recursivamente, "numa única travessia"

=== Definição dada

@

terrenosDoom :: ([Picture],[Picture],[Picture],[Picture]) -> Mapa -> (Int,Int) -> Coordenadas -> [Picture]
terrenosDoom (es,ri,re,ob) (Mapa larg []) (x,y) (xo,yo) 
    = [Blank]
terrenosDoom (es,ri,re,ob) (Mapa larg ((ter, []):t)) (x,y) (xo,yo) 
    = (terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,(yo+1)))
terrenosDoom (es,ri,re,ob) (Mapa larg ((Relva,obs):t)) (x,y) (xo,yo) 
    = (Translate 0 0 (re!!(length t))):(pictures (obstaculosDoom ob (Mapa larg ((Relva,obs):t)) (x,y) (xo,yo))):(terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,yo+1))
terrenosDoom (es,ri,re,ob) (Mapa larg ((Rio vel,obs):t)) (x,y) (xo,yo)
    = (Translate 0 0 (ri!!(length t))):(pictures (obstaculosDoom ob (Mapa larg ((Rio vel,obs):t)) (x,y) (xo,yo))):(terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,yo+1))
terrenosDoom (es,ri,re,ob) (Mapa larg ((Estrada vel,obs):t)) (x,y) (xo,yo)
    = (Translate 0 0 (es!!(length t))):(pictures (obstaculosDoom ob (Mapa larg ((Estrada vel,obs):t)) (x,y) (xo,yo))):(terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,yo+1))

@

@

    __:: ([Picture],[Picture],[Picture],[Picture])__  um tuplo de listas de pictures dado pela __'worldBuilder'__
    __-> 'Mapa'__  um 'Mapa' que a função analisa
                      recursivamente
    __-> (Int,Int)__  um tuplo de Ints que será utilizado mais tarde pela função __'obstaculosDoom'__
    __-> 'Coordenadas'__  recebe 'Coordenadas' que indicam o a ordenada onde o 'Terreno' tem de ser desenhado
    __-> [Picture]__  devolve uma lista de Pictures

@

-}

terrenosDoom :: ([Picture],[Picture],[Picture],[Picture]) -> Mapa -> (Int,Int) -> Coordenadas -> [Picture]
terrenosDoom (es,ri,re,ob) (Mapa larg []) (x,y) (xo,yo) 
    = [Blank]
terrenosDoom (es,ri,re,ob) (Mapa larg ((ter, []):t)) (x,y) (xo,yo) 
    = (terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,(yo+1)))
terrenosDoom (es,ri,re,ob) (Mapa larg ((Relva,obs):t)) (x,y) (xo,yo) 
    = (Translate 0 0 (re!!(length t))):(pictures (obstaculosDoom ob (Mapa larg ((Relva,obs):t)) (x,y) (xo,yo))):(terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,yo+1))
terrenosDoom (es,ri,re,ob) (Mapa larg ((Rio vel,obs):t)) (x,y) (xo,yo)
    = (Translate 0 0 (ri!!(length t))):(pictures (obstaculosDoom ob (Mapa larg ((Rio vel,obs):t)) (x,y) (xo,yo))):(terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,yo+1))
terrenosDoom (es,ri,re,ob) (Mapa larg ((Estrada vel,obs):t)) (x,y) (xo,yo)
    = (Translate 0 0 (es!!(length t))):(pictures (obstaculosDoom ob (Mapa larg ((Estrada vel,obs):t)) (x,y) (xo,yo))):(terrenosDoom (es,ri,re,ob) (Mapa larg t) (x,y) (0,yo+1))

{-|

== obstaculosDoom

A função __'obstaculosDoom'__ irá avaliar o 'Mapa' dado e, com auxílio da __'distancias'__ irá desenhar os 
'Obstaculo's presentes em todo o 'Mapa', tendo em noção a sua ordenada e abcissa, para criar um certo
"mundo 3D semi-realista".

=== Definição dada

@

obstaculosDoom :: [Picture] -> Mapa -> (Int,Int) -> Coordenadas -> [Picture]
obstaculosDoom ob (Mapa larg ((ter,[]):lt)) (x,y) (xo,yo) = [Blank]
obstaculosDoom ob (Mapa larg ((ter, (h:t)):lt)) (x,y) (xo,yo)
    |h /= Tronco && (y == yo && x == xo) = (distancias (xo,yo) ob 5):(obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Tronco && (y == yo && x == xo) = (distancias (xo,yo) ob 1):(distancias (xo,yo) ob 5):(obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Tronco = (distancias (xo,yo) ob 1): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Arvore = (distancias (xo,yo) ob 0): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Carro && (ter == (Estrada (velTer1 ter))) && (velTer1 ter) < 0 = (distancias (xo,yo) ob 4): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Carro && (ter == (Estrada (velTer1 ter))) && (velTer1 ter) > 0 = (distancias (xo,yo) ob 3): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |otherwise = obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo)

@

@

    __:: [Picture]__  uma lista de pictures dado pela __'terrenosDoom'__
    __-> 'Mapa'__  um 'Mapa' que a função analisa
                      recursivamente
    __-> (Int,Int)__  um tuplo de Ints que será utilizado para indicar a abcissa e ordenada do 'Jogador'
    __-> 'Coordenadas'__  recebe 'Coordenadas' que, durante o decorrer da função, irá indicar onde
                          cada 'Obstaculo' deve ser desenhado
    __-> [Picture]__  devolve uma lista de Pictures

@

-}

obstaculosDoom :: [Picture] -> Mapa -> (Int,Int) -> Coordenadas -> [Picture]
obstaculosDoom ob (Mapa larg ((ter,[]):lt)) (x,y) (xo,yo) = [Blank]
obstaculosDoom ob (Mapa larg ((ter, (h:t)):lt)) (x,y) (xo,yo)
    |h /= Tronco && (y == yo && x == xo) = (distancias (xo,yo) ob 5):(obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Tronco && (y == yo && x == xo) = (distancias (xo,yo) ob 1):(distancias (xo,yo) ob 5):(obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Tronco = (distancias (xo,yo) ob 1): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Arvore = (distancias (xo,yo) ob 0): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Carro && (ter == (Estrada (velTer1 ter))) && (velTer1 ter) < 0 = (distancias (xo,yo) ob 4): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |h == Carro && (ter == (Estrada (velTer1 ter))) && (velTer1 ter) > 0 = (distancias (xo,yo) ob 3): (obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo))
    |otherwise = obstaculosDoom ob (Mapa larg ((ter, t):lt)) (x,y) ((fromIntegral xo+1),yo)

{-|

== distancias

Dependendo de cada ordenada, esta função irá determina o tamanho de cada 'Obstaculo' ou do
'Jogador', também ditando que 'Jogador' ou 'Obstáculo' irá desenhar

=== Definição dada

@

distancias :: Coordenadas -> [Picture] -> Int -> Picture
distancias (xo,yo) ob z
    |yo == 0 = (scale 0.3   0.3  (Translate (350*(fromIntegral (xo - 3))) (1520) (ob!!z)))
    |yo == 1 = (scale 0.35  0.35 (Translate (335*(fromIntegral (xo - 3))) (1215) (ob!!z)))
    |yo == 2 = (scale 0.4   0.4  (Translate (325*(fromIntegral (xo - 3))) (960)  (ob!!z)))
    |yo == 3 = (scale 0.45  0.45 (Translate (315*(fromIntegral (xo - 3))) (740)  (ob!!z)))
    |yo == 4 = (scale 0.5   0.5  (Translate (315*(fromIntegral (xo - 3))) (540)  (ob!!z)))
    |yo == 5 = (scale 0.6   0.6  (Translate (300*(fromIntegral (xo - 3))) (315)  (ob!!z)))
    |yo == 6 = (scale 0.8   0.8  (Translate (265*(fromIntegral (xo - 3))) (120)  (ob!!z)))
    |yo == 7 = (scale 1     1    (Translate (245*(fromIntegral (xo - 3))) (-30)  (ob!!z)))
    |yo == 8 = (scale 1.25  1.25 (Translate (240*(fromIntegral (xo - 3))) (-175) (ob!!z)))

@

@

    __:: 'Coordenadas'__  usadas para perceber em que ordenada se está a trabalhar e para
                          desenhar na dada abcissa e ordenada
    __-> [Picture]__  uma lista de pictures dado pela __'obstaculosDoom'__
    __-> Int__  um Int usado para determinar que elemento da lista de Pictures será desenhado
    __-> Picture__  devolve uma Picture

@

-}

distancias :: Coordenadas -> [Picture] -> Int -> Picture
distancias (xo,yo) ob z
    |yo == 0 = (scale 0.3   0.3  (Translate (350*(fromIntegral (xo - 3))) (1520) (ob!!z)))
    |yo == 1 = (scale 0.35  0.35 (Translate (335*(fromIntegral (xo - 3))) (1215) (ob!!z)))
    |yo == 2 = (scale 0.4   0.4  (Translate (325*(fromIntegral (xo - 3))) (960)  (ob!!z)))
    |yo == 3 = (scale 0.45  0.45 (Translate (315*(fromIntegral (xo - 3))) (740)  (ob!!z)))
    |yo == 4 = (scale 0.5   0.5  (Translate (315*(fromIntegral (xo - 3))) (540)  (ob!!z)))
    |yo == 5 = (scale 0.6   0.6  (Translate (300*(fromIntegral (xo - 3))) (315)  (ob!!z)))
    |yo == 6 = (scale 0.8   0.8  (Translate (265*(fromIntegral (xo - 3))) (120)  (ob!!z)))
    |yo == 7 = (scale 1     1    (Translate (245*(fromIntegral (xo - 3))) (-30)  (ob!!z)))
    |yo == 8 = (scale 1.25  1.25 (Translate (240*(fromIntegral (xo - 3))) (-175) (ob!!z)))

-----------------------------------------------------------------GLOSS 3D COMPLICADO-----------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------------

{-|

== worldReaction

Esta função vai, maioritariamente, responder ao pressionar de teclas esperadas, alterando a
lista de Ints mencionada inicialmente do 'Menu', nos casos em que não afeta esta lista,
irá responder imediatamente.
Esta está incrivelmente associada à __'tokioTomare'__, fazendo com que tenha um papel
igualmente importante que __'worldReaction'__ 

=== Definição dada

@

worldReaction :: Event -> WORLD -> WORLD
worldReaction (EventKey key Down _ _) ((Menu u Starting o seed,[0],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyUp                                                                   = ((Menu u Starting o seed,[1],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyDown                                                                 = ((Menu u Starting o seed,[2],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyEnter && o == 0                                                      = (((Menu u (Queueing (0,0)) 0) seed,[0],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyEnter && o == 1 && seed == []                                        = ((Gamer u Doom (superRandom) (False),[0],(xo,yo)), (obs,(jogoInicial (superRandom) Doom),0,(r-1)))    
    | key == SpecialKey KeyEnter && o == 1 && not (elem False (map isDigit seed)) && seed /= [] = ((Gamer u Doom (read seed) (False),[0],(xo,yo)), (obs,(jogoInicial (read seed) Doom),0,r))
    | key == SpecialKey KeyEnter && o == 2                                                      = ((Menu u Instrucoes 0 seed,[0],(xo,yo)), (obs,j,f,r))
    | key == Char 'q' || (key == SpecialKey KeyEnter && o == 3)                                 = error "Fim"
    | key == SpecialKey KeyEnter && o == 4 && seed == []                                        = ((Gamer u TRUE_CLASSIC (superRandom) (False),[0],(xo,yo)), (obs,(jogoInicial (superRandom) TRUE_CLASSIC),0,(r-1)))    
    | key == SpecialKey KeyEnter && o == 4 && not (elem False (map isDigit seed)) && seed /= [] = ((Gamer u TRUE_CLASSIC (read seed) (False),[0],(xo,yo)), (obs,(jogoInicial (read seed) TRUE_CLASSIC),0,(r)))    
                where superRandom = mod (head (randoms (mkStdGen r))) 10000000000

worldReaction (EventKey key Down _ _) (((Menu un (Queueing (d,u)) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyLeft                                                                      = (((Menu un (Queueing (d,u)) k seed),[1],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyRight                                                                     = (((Menu un (Queueing (d,u)) k seed),[2],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyUp && k == 0                                                              = (((Menu un (Queueing (d,(mod (u+1) 10))) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyDown && k == 0                                                            = (((Menu un (Queueing (d,(mod (u-1) 10))) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyUp && k == 1                                                              = (((Menu un (Queueing ((mod (d+1) 10),u)) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyDown && k == 1                                                            = (((Menu un (Queueing ((mod (d-1) 10),u)) k seed),[n],(xo,yo)), (obs,j,f,r))
    | key == Char 'q'                                                                                = ((Menu un Starting 0 seed,[0],(0,0)), (obs,j,0,r))
    | key == SpecialKey KeyEnter && largura > 2 && seed == []                                        = ((Gamer un (Classic largura) (superRandom) (False),[0],(xo,yo)), (obs,(jogoInicial (superRandom) (Classic largura)),0,(r-1))) 
    | key == SpecialKey KeyEnter && largura > 2 && not (elem False (map isDigit seed)) && seed /= [] = ((Gamer un (Classic largura) (read seed) (False),[0],(xo,yo)), (obs,(jogoInicial (read seed) (Classic largura)),0,r)) 
        where largura = u + d*10  
              superRandom = mod (head (randoms (mkStdGen r))) 10000000000

worldReaction (EventKey key Down _ _) ((Menu u Instrucoes k seed,[n],(xo,yo)), (obs,j,f,r))
    |key == Char 'q'                                           = ((Menu u Starting 2 seed,[0],(xo,yo)), (obs,j,f,r))
    |key == SpecialKey KeyRight                                = ((Menu u Instrucoes k seed,[1],(xo,yo)), (obs,j,f,r))
    |key == SpecialKey KeyLeft                                 = ((Menu u Instrucoes k seed,[2],(xo,yo)), (obs,j,f,r))
worldReaction (EventKey (Char n) Down _ _) ((Menu u m k seed,[0],(xo,yo)), (obs,j,f,r))               
    | length seed < 10 && elem n ['0'..'9'] || (u == Locked && ((length seed < 7 && (toLower n) == ("frogger" !! (length seed))))) 
                     = ((Menu u m k (seed++[n]),[0],(xo,yo)), (obs,j,f,r)) 
worldReaction (EventKey (SpecialKey KeyDelete) Down _ _) ((Menu u m k seed,[0],(xo,yo)), (obs,j,f,r)) 
    | seed /= []     = ((Menu u m k (init seed),[0],(xo,yo)), (obs,j,f,r)) 

worldReaction (EventKey key Down _ _) ((Gamer u (Classic l) wandom (_),[n],(xo,yo)), (obs,j,f,r)) 
    |key == Char 'q' = (((Menu u (Queueing ((div l 10),(mod l 10))) 0 (show wandom)),[0],(0,0)), (obs,j,0,r))

worldReaction (EventKey key Down _ _) ((Gamer u mode wandom gamend,[n],(xo,yo)), (obs,j,f,r)) 
    | gamend == False && key == SpecialKey KeyUp    = ((Gamer u mode wandom gamend,[1],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeyDown  = ((Gamer u mode wandom gamend,[2],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeyLeft  = ((Gamer u mode wandom gamend,[3],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeyRight = ((Gamer u mode wandom gamend,[4],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeySpace = ((Gamer u mode wandom gamend,[(abs n)-1],(xo,yo)), (obs,j,f,r))
    | key == Char 'r'                               = ((Gamer u mode wandom (False),[0],(xo,yo)), (obs,(jogoInicial wandom mode),0,r))
    | key == Char 'q' && mode == (Doom)             = (((Menu u Starting 1 (show wandom)),[0],(0,0)), (obs,j,0,r))
    | key == Char 'q' && mode == (TRUE_CLASSIC)     = (((Menu u Starting 4 (show wandom)),[0],(0,0)), (obs,j,0,r))
worldReaction _ ((s,[-1],(xo,yo)), e)               = ((s,[-1],(xo,yo)), e)
worldReaction _ ((s,_,(xo,yo)), e)                  = ((s,[0],(xo,yo)), e)

@

@

    __:: Event__  recebe um Event
    __-> 'WORLD'__  recebe um 'WORLD'
    __-> 'WORLD'__ devolve um 'WORLD'

@

-}

worldReaction :: Event -> WORLD -> WORLD
worldReaction (EventKey key Down _ _) ((Menu u Starting o seed,[0],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyUp                                                                   = ((Menu u Starting o seed,[1],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyDown                                                                 = ((Menu u Starting o seed,[2],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyEnter && o == 0                                                      = (((Menu u (Queueing (0,0)) 0) seed,[0],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyEnter && o == 1 && seed == []                                        = ((Gamer u Doom (superRandom) (False),[0],(xo,yo)), (obs,(jogoInicial (superRandom) Doom),0,(r-1)))    
    | key == SpecialKey KeyEnter && o == 1 && not (elem False (map isDigit seed)) && seed /= [] = ((Gamer u Doom (read seed) (False),[0],(xo,yo)), (obs,(jogoInicial (read seed) Doom),0,r))
    | key == SpecialKey KeyEnter && o == 2                                                      = ((Menu u Instrucoes 0 seed,[0],(xo,yo)), (obs,j,f,r))
    | key == Char 'q' || (key == SpecialKey KeyEnter && o == 3)                                 = error "Fim"
    | key == SpecialKey KeyEnter && o == 4 && seed == []                                        = ((Gamer u TRUE_CLASSIC (superRandom) (False),[0],(xo,yo)), (obs,(jogoInicial (superRandom) TRUE_CLASSIC),0,(r-1)))    
    | key == SpecialKey KeyEnter && o == 4 && not (elem False (map isDigit seed)) && seed /= [] = ((Gamer u TRUE_CLASSIC (read seed) (False),[0],(xo,yo)), (obs,(jogoInicial (read seed) TRUE_CLASSIC),0,(r)))    
                where superRandom = mod (head (randoms (mkStdGen r))) 10000000000

worldReaction (EventKey key Down _ _) (((Menu un (Queueing (d,u)) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyLeft                                                                      = (((Menu un (Queueing (d,u)) k seed),[1],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyRight                                                                     = (((Menu un (Queueing (d,u)) k seed),[2],(xo,yo)), (obs,j,f,r))
    | key == SpecialKey KeyUp && k == 0                                                              = (((Menu un (Queueing (d,(mod (u+1) 10))) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyDown && k == 0                                                            = (((Menu un (Queueing (d,(mod (u-1) 10))) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyUp && k == 1                                                              = (((Menu un (Queueing ((mod (d+1) 10),u)) k seed),[n],(xo,yo)), (obs,j,f,r)) 
    | key == SpecialKey KeyDown && k == 1                                                            = (((Menu un (Queueing ((mod (d-1) 10),u)) k seed),[n],(xo,yo)), (obs,j,f,r))
    | key == Char 'q'                                                                                = ((Menu un Starting 0 seed,[0],(0,0)), (obs,j,0,r))
    | key == SpecialKey KeyEnter && largura > 2 && seed == []                                        = ((Gamer un (Classic largura) (superRandom) (False),[0],(xo,yo)), (obs,(jogoInicial (superRandom) (Classic largura)),0,(r-1))) 
    | key == SpecialKey KeyEnter && largura > 2 && not (elem False (map isDigit seed)) && seed /= [] = ((Gamer un (Classic largura) (read seed) (False),[0],(xo,yo)), (obs,(jogoInicial (read seed) (Classic largura)),0,r)) 
        where largura = u + d*10  
              superRandom = mod (head (randoms (mkStdGen r))) 10000000000

worldReaction (EventKey key Down _ _) ((Menu u Instrucoes k seed,[n],(xo,yo)), (obs,j,f,r))
    |key == Char 'q'                                           = ((Menu u Starting 2 seed,[0],(xo,yo)), (obs,j,f,r))
    |key == SpecialKey KeyRight                                = ((Menu u Instrucoes k seed,[1],(xo,yo)), (obs,j,f,r))
    |key == SpecialKey KeyLeft                                 = ((Menu u Instrucoes k seed,[2],(xo,yo)), (obs,j,f,r))
worldReaction (EventKey (Char n) Down _ _) ((Menu u m k seed,[0],(xo,yo)), (obs,j,f,r))               
    | length seed < 10 && elem n ['0'..'9'] || (u == Locked && ((length seed < 7 && (toLower n) == ("frogger" !! (length seed))))) 
                     = ((Menu u m k (seed++[n]),[0],(xo,yo)), (obs,j,f,r)) 
worldReaction (EventKey (SpecialKey KeyDelete) Down _ _) ((Menu u m k seed,[0],(xo,yo)), (obs,j,f,r)) 
    | seed /= []     = ((Menu u m k (init seed),[0],(xo,yo)), (obs,j,f,r)) 

worldReaction (EventKey key Down _ _) ((Gamer u (Classic l) wandom (_),[n],(xo,yo)), (obs,j,f,r)) 
    |key == Char 'q' = (((Menu u (Queueing ((div l 10),(mod l 10))) 0 (show wandom)),[0],(0,0)), (obs,j,0,r))

worldReaction (EventKey key Down _ _) ((Gamer u mode wandom gamend,[n],(xo,yo)), (obs,j,f,r)) 
    | gamend == False && key == SpecialKey KeyUp    = ((Gamer u mode wandom gamend,[1],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeyDown  = ((Gamer u mode wandom gamend,[2],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeyLeft  = ((Gamer u mode wandom gamend,[3],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeyRight = ((Gamer u mode wandom gamend,[4],(xo,yo)), (obs,j,f,r))
    | gamend == False && key == SpecialKey KeySpace = ((Gamer u mode wandom gamend,[(abs n)-1],(xo,yo)), (obs,j,f,r))
    | key == Char 'r'                               = ((Gamer u mode wandom (False),[0],(xo,yo)), (obs,(jogoInicial wandom mode),0,r))
    | key == Char 'q' && mode == (Doom)             = (((Menu u Starting 1 (show wandom)),[0],(0,0)), (obs,j,0,r))
    | key == Char 'q' && mode == (TRUE_CLASSIC)     = (((Menu u Starting 4 (show wandom)),[0],(0,0)), (obs,j,0,r))
worldReaction _ ((s,[-1],(xo,yo)), e)               = ((s,[-1],(xo,yo)), e)
worldReaction _ ((s,_,(xo,yo)), e)                  = ((s,[0],(xo,yo)), e)

{-|

== tokioTomare

Esta função vai, não só funcionar como uma espécie de "contador global", como também
funcionará como parte da __'worldReaction'__, contando o "tempo" que se passa no 'Jogo',
o que, consequentemente, fará com que os 'Obstaculo's se "deslizem suavemente" no 'Mode'
'Classic', com que os 'Obstaculo's em geral se movimentem, com que a __'deslizaJogo'__ 
funcione de x em x segundos, com que o 'Jogo' continue "Pausado" sem ter de manter a 
tecla "Espaço" pressionada, etc.

=== Definição dada

@

tokioTomare :: Float -> WORLD -> WORLD
tokioTomare _ ((Gamer u mode wandom (True),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r)) = ((Gamer u mode wandom (True),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r))
tokioTomare n ((Gamer u mode wandom (False),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r)) 
    |jogoTerminou (Jogo (Jogador (x, y)) (Mapa larg terobs)) || (jogoTerminou (Jogo (Jogador (novoX, novoY)) (Mapa larg novoTerObs)) && ((snd(terobs !! y))!! x /= Carro))                         = ((Gamer u mode wandom (True),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r)) 
    |(round b) /= 500 && (mod (round b) ((round n)*600) == 500) 
        || ((mode == Doom || mode == TRUE_CLASSIC) && y == 4) 
        || (mode == Classic larg && y == round ((fromIntegral (alturaMapa larg))/3)) = ((Gamer u mode wandom (False),[k],(xo,yo)), (obstaculos,(deslizaJogo (wandom+((round b)*7)) (Jogo (Jogador (x,y)) (Mapa larg novoTerObs))),b+n,r)) 
    |k == 1                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Cima) (x,y) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == 2                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Baixo) (x,y) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == 3                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Esquerda) (novoX,novoY) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == 4                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Direita) (novoX,novoY) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == -1                                                                         = ((Gamer u mode wandom (False),[-1],(xo,yo)), (obstaculos,(Jogo (Jogador (x,y)) (Mapa larg novoTerObs)),b,r))
    |k == 0                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (novoX,novoY)) (Mapa larg novoTerObs)),b+n,r))
                    where n | b < 12000                 = 5
                            | b >= 12000 && b < 60000   = 10
                            | b >= 60000 && b < 120000  = 12.5
                            | b >= 120000 && b < 240000 = 20
                            | b >= 240000               = 25
                          xo = (x*90) - 45*(larg-1)
                          yo = - (y*90) + 45*((alturaMapa larg)-1)
                          (novoTerObs,(novoX,novoY)) = moveObsCorrente terobs (x,y) (b)
tokioTomare _ ((Menu Locked mode o seed,[k],(xo,yo)), (obstaculos,j,b,r)) 
    | map toLower seed == "frogger" = ((Menu Frogger mode o "",[k],(xo,yo)), (obstaculos,j,b,r)) 
tokioTomare _ ((Menu u Starting o seed,[k],(xo,yo)), (obstaculos,j,b,r)) 
    |k == 0 = ((Menu u Starting o seed,[0],(xo,yo)), (obstaculos,j,b,r))
    |k == 1 = ((Menu u Starting (mod (o-1) froggerAvailable) seed,[0],(xo,yo)), (obstaculos,j,b,r))
    |k == 2 = ((Menu u Starting (mod (o+1) froggerAvailable) seed,[0],(xo,yo)), (obstaculos,j,b,r))
        where froggerAvailable | u == Locked  = 4
                               | u == Frogger = 5
tokioTomare _ (((Menu un (Queueing (d,u)) k seed),[n],(xo,yo)), (obs,j,f,r))
    |n == 0 = (((Menu un (Queueing (d,u)) k seed),[0],(xo,yo)), (obs,j,f,r))
    |n == 1 = (((Menu un (Queueing (d,u)) (mod (k+1) 2) seed) ,[0],(xo,yo)), (obs,j,f,r))
    |n == 2 = (((Menu un (Queueing (d,u)) (mod (k-1) 2) seed),[0],(xo,yo)), (obs,j,f,r))
tokioTomare _ ((Menu u Instrucoes k seed,[n],(xo,yo)), (obs,j,f,r))
    |n == 0 = ((Menu u Instrucoes k seed,[n],(xo,yo)), (obs,j,f,r))
    |n == 1 = ((Menu u Instrucoes (mod (k+1) 4) seed,[0],(xo,yo)), (obs,j,f,r))
    |n == 2 = ((Menu u Instrucoes (mod (k-1) 4) seed,[0],(xo,yo)), (obs,j,f,r))

@

@

    __:: Float__  recebe um Float
    __-> 'WORLD'__  recebe um 'WORLD'
    __-> 'WORLD'__ devolve um 'WORLD'

@

-}

tokioTomare :: Float -> WORLD -> WORLD
tokioTomare _ ((Gamer u mode wandom (True),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r)) = ((Gamer u mode wandom (True),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r))
tokioTomare n ((Gamer u mode wandom (False),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r)) 
    |jogoTerminou (Jogo (Jogador (x, y)) (Mapa larg terobs)) || (jogoTerminou (Jogo (Jogador (novoX, novoY)) (Mapa larg novoTerObs)) && ((snd(terobs !! y))!! x /= Carro))                         = ((Gamer u mode wandom (True),[k],(xo,yo)), (obstaculos,(Jogo (Jogador (x, y)) (Mapa larg terobs)),b,r)) 
    |(round b) /= 500 && (mod (round b) ((round n)*600) == 500) 
        || ((mode == Doom || mode == TRUE_CLASSIC) && y == 4) 
        || (mode == Classic larg && y == round ((fromIntegral (alturaMapa larg))/3)) = ((Gamer u mode wandom (False),[k],(xo,yo)), (obstaculos,(deslizaJogo (wandom+((round b)*7)) (Jogo (Jogador (x,y)) (Mapa larg novoTerObs))),b+n,r)) 
    |k == 1                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Cima) (x,y) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == 2                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Baixo) (x,y) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == 3                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Esquerda) (novoX,novoY) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == 4                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (limites terobs (Direita) (novoX,novoY) larg)) (Mapa larg novoTerObs)),b+n,r))
    |k == -1                                                                         = ((Gamer u mode wandom (False),[-1],(xo,yo)), (obstaculos,(Jogo (Jogador (x,y)) (Mapa larg novoTerObs)),b,r))
    |k == 0                                                                          = ((Gamer u mode wandom (False),[0],(xo,yo)), (obstaculos,(Jogo (Jogador (novoX,novoY)) (Mapa larg novoTerObs)),b+n,r))
                    where n | b < 12000                 = 5
                            | b >= 12000 && b < 60000   = 10
                            | b >= 60000 && b < 120000  = 12.5
                            | b >= 120000 && b < 240000 = 20
                            | b >= 240000               = 25
                          xo = (x*90) - 45*(larg-1)
                          yo = - (y*90) + 45*((alturaMapa larg)-1)
                          (novoTerObs,(novoX,novoY)) = moveObsCorrente terobs (x,y) (b)
tokioTomare _ ((Menu Locked mode o seed,[k],(xo,yo)), (obstaculos,j,b,r)) 
    | map toLower seed == "frogger" = ((Menu Frogger mode o "",[k],(xo,yo)), (obstaculos,j,b,r)) 
tokioTomare _ ((Menu u Starting o seed,[k],(xo,yo)), (obstaculos,j,b,r)) 
    |k == 0 = ((Menu u Starting o seed,[0],(xo,yo)), (obstaculos,j,b,r))
    |k == 1 = ((Menu u Starting (mod (o-1) froggerAvailable) seed,[0],(xo,yo)), (obstaculos,j,b,r))
    |k == 2 = ((Menu u Starting (mod (o+1) froggerAvailable) seed,[0],(xo,yo)), (obstaculos,j,b,r))
        where froggerAvailable | u == Locked  = 4
                               | u == Frogger = 5
tokioTomare _ (((Menu un (Queueing (d,u)) k seed),[n],(xo,yo)), (obs,j,f,r))
    |n == 0 = (((Menu un (Queueing (d,u)) k seed),[0],(xo,yo)), (obs,j,f,r))
    |n == 1 = (((Menu un (Queueing (d,u)) (mod (k+1) 2) seed) ,[0],(xo,yo)), (obs,j,f,r))
    |n == 2 = (((Menu un (Queueing (d,u)) (mod (k-1) 2) seed),[0],(xo,yo)), (obs,j,f,r))
tokioTomare _ ((Menu u Instrucoes k seed,[n],(xo,yo)), (obs,j,f,r))
    |n == 0 = ((Menu u Instrucoes k seed,[n],(xo,yo)), (obs,j,f,r))
    |n == 1 = ((Menu u Instrucoes (mod (k+1) 4) seed,[0],(xo,yo)), (obs,j,f,r))
    |n == 2 = ((Menu u Instrucoes (mod (k-1) 4) seed,[0],(xo,yo)), (obs,j,f,r))

{-|

== moveObsCorrente

Esta função é uma complementada pela __'moveObs'__ e, tem como objetivo principal, perceber se o 'Jogador' se encontra
num 'Tronco' e responde corretamente caso seja verdade (também respondendo à Adenda da Tarefa3 quando complementada
pela __'moveObs'__).

=== Definição dada

@

moveObsCorrente :: [(Terreno,[Obstaculo])] -> Coordenadas  -> Float -> ([(Terreno,[Obstaculo])],Coordenadas)
moveObsCorrente terObs (x,y) b 
    |((snd(terObs !! y)) !! x == Tronco) && vel1Val 
        = ((moveObs terObs (x + (signum (velTer1(fst (terObs !! y)))),y) b),(x + (signum (velTer1(fst (terObs !! y)))),y))
    |otherwise                                      
        = ((moveObs terObs (x,y) b),(x,y))
        where vel1Val = mod (round b) ((4-(abs (velTer1(fst (terObs !! y)))))*200) == 0

@

@

    __:: [('Terreno',['Obstaculo'])]__  recebe um tuplo de 'Terreno's e lista de 'Obstaculo's
    __-> 'Coordenadas'__  recebe as 'Coordenadas' do 'Jogador'
    __-> Float__ recebe o Float indicativo do tempo atual do 'Jogo'
    __-> ([('Terreno',['Obstaculo'])],'Coordenadas')__ devolve um tuplo que contém: uma lista de tuplos de 'Terreno's e listas 
                                                       de 'Obstaculo's; e as novas 'Coordenadas' do 'Jogador'

@

-}

moveObsCorrente :: [(Terreno,[Obstaculo])] -> Coordenadas  -> Float -> ([(Terreno,[Obstaculo])],Coordenadas)
moveObsCorrente terObs (x,y) b 
    |((snd(terObs !! y)) !! x == Tronco) && vel1Val 
        = ((moveObs terObs (x + (signum (velTer1(fst (terObs !! y)))),y) b),(x + (signum (velTer1(fst (terObs !! y)))),y))
    |otherwise                                      
        = ((moveObs terObs (x,y) b),(x,y))
        where vel1Val = mod (round b) ((4-(abs (velTer1(fst (terObs !! y)))))*200) == 0

{-|

== moveObs

Esta função é uma "versão melhorada" de certas funções do módulo __'Tarefa3_2022li1g124'__,
sendo que, da maneira como o módulo tratava do movimento dos 'Obstaculo's, o Usuário poderia
vir a confundir-se com o movimento de certos 'Obstaculo's a certas 'Velocidade's
e.g.
    Numa 'Estrada' repleto de 'Carro's intervalados ([Nenhum,Carro,Nenhum,Nenhum,Carro,Nenhum]),
    o 'Jogador' poderia tentar atravessar a 'Estrada', mas se a 'Velocidade' fosse de 3, o 'Jogador'
    não seria capaz de se mover horizontalmente nesta, pois os 'Carro's andariam de 3 em 3 Blocos.

=== Definição dada

@

moveObs :: [(Terreno,[Obstaculo])] -> Coordenadas -> Float -> [(Terreno,[Obstaculo])]
moveObs [] _ _ = []
moveObs ((ter, (l:lt)):t) (x,y) b
    |(((y == 0) && ((l:lt) !! x == Carro))) = (ter, (l:lt)):(moveObs t (x,(y-1)) b)        
    |(((velTer1 ter) > 0) && vel1Val)       = (ter, ((last lt):(init (l:lt)))):(moveObs t (x,(y-1)) b)
    |(((velTer1 ter) < 0) && vel1Val)       = (ter, ((lt)++[l])):(moveObs t (x,(y-1)) b)
    |otherwise                              = (ter, (l:lt)):(moveObs t (x,(y-1)) b)
        where vel1Val = mod (round b) ((4-(abs (velTer1 ter)))*200) == 0

@

@

    __:: [('Terreno',['Obstaculo'])]__  recebe um tuplo de 'Terreno's e lista de 'Obstaculo's
    __-> 'Coordenadas'__  recebe as 'Coordenadas' do 'Jogador'
    __-> Float__ recebe o Float indicativo do tempo atual do 'Jogo'
    __-> [('Terreno',['Obstaculo'])]__ devolve uma lista de tuplos de 'Terreno's e listas de 'Obstaculo's

@

-}

moveObs :: [(Terreno,[Obstaculo])] -> Coordenadas -> Float -> [(Terreno,[Obstaculo])]
moveObs [] _ _ = []
moveObs ((ter, (l:lt)):t) (x,y) b
    |(((y == 0) && ((l:lt) !! x == Carro))) = (ter, (l:lt)):(moveObs t (x,(y-1)) b)        
    |(((velTer1 ter) > 0) && vel1Val)       = (ter, ((last lt):(init (l:lt)))):(moveObs t (x,(y-1)) b)
    |(((velTer1 ter) < 0) && vel1Val)       = (ter, ((lt)++[l])):(moveObs t (x,(y-1)) b)
    |otherwise                              = (ter, (l:lt)):(moveObs t (x,(y-1)) b)
        where vel1Val = mod (round b) ((4-(abs (velTer1 ter)))*200) == 0
----------------------------------------------------------MAPA INICAL RANDOMIZADO---------------------------------------------------------

{-|

== jogoInicial

Apesar de querer um 'Jogo' aleatório constantemente, decidi que era melhor o Usuário ser capaz de "escolher"
o 'Jogo' inicial, para caso esteja a entender muito bem o programa, se possa familiarizar com uma certa seed,
seja esta escolhida por ele, ou pela Main, e que após perceber bem esta certa seed, possa continuar para o
'Jogo', funcionando como uma espécie de "tutorial aleatório".
Dado um certo Int e 'Mode', ditará qual deverá ser a posição inicial do 'Jogador' e a "altura do mapa".
Esta função será complementada pela __'mapaEstendidoInicial'__

=== Definição dada

@

jogoInicial :: Int -> Mode -> Jogo
jogoInicial wan Undefined    = (Jogo (Jogador (0,0)) (Mapa 1 [(Relva, [Nenhum])]))
jogoInicial wan (Classic l)  = (Jogo (Jogador ((round (((fromIntegral l)-1)/2)),((((fromIntegral (alturaMapa l))-1))))) (mapaEstendidoInicial (Mapa l []) wan (alturaMapa l)))
jogoInicial wan Doom         = (Jogo (Jogador (3,8)) (mapaEstendidoInicial (Mapa 7 []) wan 9))
jogoInicial wan TRUE_CLASSIC = (Jogo (Jogador (4,11)) (mapaEstendidoInicial (Mapa 9 []) wan 12))

@

@

    __:: Int__  recebe um Int que será atribuído pela Main aleatoriamente, ou pelo Usuário
    __-> 'Mode'__  recebe um 'Mode' para devolver um 'Jogo' apropriado do 'Jogador'
    __-> 'Jogo'__ devolve um 'Jogo'

@

-}

jogoInicial :: Int -> Mode -> Jogo
jogoInicial wan Undefined    = (Jogo (Jogador (0,0)) (Mapa 1 [(Relva, [Nenhum])]))
jogoInicial wan (Classic l)  = (Jogo (Jogador ((round (((fromIntegral l)-1)/2)),((((fromIntegral (alturaMapa l))-1))))) (mapaEstendidoInicial (Mapa l []) wan (alturaMapa l)))
jogoInicial wan Doom         = (Jogo (Jogador (3,8)) (mapaEstendidoInicial (Mapa 7 []) wan 9))
jogoInicial wan TRUE_CLASSIC = (Jogo (Jogador (4,11)) (mapaEstendidoInicial (Mapa 9 []) wan 12))
-------------------------------------------------------------------------------------------------------------------------------------------------

{-|

== mapaEstendidoInicial

Complementando a __'jogoInicial'__, esta função, vai ditar o 'Mapa' criado, tendo em conta a "altura" que este deve ter,
e que 'Mapa' deve criar, tendo em consideração o 1º Int recebido

=== Definição dada

@

mapaEstendidoInicial :: Mapa -> Int -> Int -> Mapa
mapaEstendidoInicial (Mapa l terobs) (wandom) k
    |length terobs == k = (Mapa l terobs)
    |otherwise          = mapaEstendidoInicial (estendeMapa (Mapa l terobs) (wandom)) ((wandom+1)*9) k

@

@

    __:: 'Mapa'__  recebe um 'Mapa' que será estendido até ter uma "altura correta"
    __-> Int__  recebe um Int, que irá ditar que 'Mapa' será gerado
    __-> Int__ recebe um segundo Int, que irá tratar da altura do 'Mapa'
    __-> 'Mapa'__ devolve um 'Mapa' 

@

-}

mapaEstendidoInicial :: Mapa -> Int -> Int -> Mapa
mapaEstendidoInicial (Mapa l terobs) (wandom) k
    |length terobs == k = (Mapa l terobs)
    |otherwise          = mapaEstendidoInicial (estendeMapa (Mapa l terobs) (wandom)) ((wandom+1)*9) k
------------------------------------------------------------MANAGER DE CADA MODO DE JOGO-----------------------------------------------------------

{-|

== alturaMapa

Esta função só tem como objetivo responder ao 'Mode' 'Classic', sendo que para ser algo jogável,
a altura não pode ser sempre a mesma, porque assim o 'Jogo' acabava por ser a mesma coisa, com
mais alguma "horizontalidade"

=== Definição dada

@

alturaMapa :: Int -> Int
alturaMapa l | l < 20   = 10
             |otherwise = round (fromIntegral(l)*0.55)

@

@

    __:: Int__  recebe um Int, correspondente à largura dada
    __-> Int__  devolve um Int apropriado para o Int recebido

@

-}

alturaMapa :: Int -> Int
alturaMapa l | l < 20   = 10
             |otherwise = round (fromIntegral(l)*0.55)
--------------------------------------------------------------------------------------------------------

{-|

== fr

Esta função só serve para caso mais tarde, achar apropriado alterar o número
de simulações por segundo feitas pelo programa.
Apenas indicará um Int que a Main irá usar constantemente

=== Definição dada

@

fr:: Int
fr = 60

@

@

    __:: Int__  afirma um Int

@

-}

fr:: Int
fr = 60

{-|

== dm

Esta função indica que o tamanho da janela, será 'FullScreen'

=== Definição dada

@

dm :: Display
dm =  FullScreen 

@

@

    __:: Int__  afirma um Int

@

-}

dm :: Display
dm =  FullScreen 

