import Text.Show.Functions

-- Modelo inicial
data Jugador = UnJugador {
  nombre :: String,
  padre :: String,
  habilidad :: Habilidad
} deriving (Eq, Show)

data Habilidad = Habilidad {
  fuerzaJugador :: Int,
  precisionJugador :: Int
} deriving (Eq, Show)

-- Jugadores de ejemplo
bart = UnJugador "Bart" "Homero" (Habilidad 25 60)
todd = UnJugador "Todd" "Ned" (Habilidad 15 80)
rafa = UnJugador "Rafa" "Gorgory" (Habilidad 10 1)

data Tiro = UnTiro {
  velocidad :: Int,
  precision :: Int,
  altura :: Int
} deriving (Eq, Show)

type Puntos = Int

-- Funciones útiles
between n m x = elem x [n .. m]

maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b


----PUNTO 1----

{-a. Modelar los palos usados en el juego que a partir de una determinada habilidad generan un tiro que se compone por velocidad, precisión y altura.
i. El putter genera un tiro con velocidad igual a 10, el doble de la precisión recibida y altura 0.
ii. La madera genera uno de velocidad igual a 100, altura igual a 5 y la mitad de la precisión.
iii. Los hierros, que varían del 1 al 10 (número al que denominaremos n), generan un tiro de velocidad igual a la fuerza 
multiplicada por n, la precisión dividida por n y una altura de n-3 (con mínimo 0). Modelarlos de la forma más genérica posible.   -}

type Palo = Hbailidad -> Tiro

putter :: Palo 
putter habilidad = unTiro { velocidad = 10, precision = precisionJugador habilidad * 2, altura = 0 }

madera :: Palo 
madera habilidad = unTiro { velocidad = 100, precision = precisionJugador habilidad 'div' 2, altura = 5 }

hierro :: Int -> Palo
hierro n habilidad = unTiro { 
  velocidad = fuerzaJugadorhabilidad * n, 
  precision = precisionJugador habilidad 'div' n,
  altura = (n - 3) 'max' 0 
}

--b. Definir una constante palos que sea una lista con todos los palos que se pueden usar en el juego.

palos :: [Palo] --Son 12 palos en total 
palos = [putter , madera] ++ map hierro [1..10]



----PUNTO 2----

{-Definir la función golpe que dados una persona y un palo, obtiene el tiro resultante de usar ese palo con las habilidades 
de la persona. Por ejemplo si Bart usa un putter, se genera un tiro de velocidad = 10, precisión = 120 y altura = 0. -}

golpe :: Jugador -> Palo -> Tiro 
golpe UnJugador unPalo = unPalo (habilidad unJugador) --El tipo espera una habilidad de un jugador y devuelve un tiro 

--otra froma de hacerlo...
golpe' :: Jugador -> Palo -> Tiro 
golpe' UnJugador unPalo = (unPalo . habilidad) unJugador

--otra froma de hacerlo...
golpe'' :: Palo -> Jugador -> Tiro 
golpe'' unPalo = unPalo . habilidad --Usando point free, doy vuelta el tipo de la fucnión (espera un palo y devuelve el jugador con su tiro)



----PUNTO 3----

{-Obstaculos: si un tiro puede superarlo, y en el caso de poder superarlo, cómo se ve afectado dicho tiro por el obstáculo. 
Se desea saber cómo queda un tiro luego de intentar superar un obstáculo, teniendo en cuenta que en caso de no superarlo, 
se detiene, quedando con todos sus componentes en 0.   

a. Un túnel con rampita sólo es superado si la precisión es mayor a 90 yendo al ras del suelo, independientemente de la velocidad 
del tiro. Al salir del túnel la velocidad del tiro se duplica, la precisión pasa a ser 100 y la altura 0. -}

--type Obstaculo = Tiro -> Tiro 

data Obstaculo = unObstaculo {
  puedeSuperar :: Tiro -> Bool,
  efectoLuegoDeSuperar :: Tiro -> Tiro 
}

{-obstaculoSuperbleSi :: (Tiro -> Bool) -> (Tiro -> Tiro) -> Obstaculo 
obstaculoSuperbleSi condicion efecto unTiro 
  | condicion unTiro = efecto unTiro
  | otherwise = tiroDetenido -}

intentarSuperarObstaculo :: Obstaculo -> Tiro -> Tiro
intentarSuperarObstaculo obstaculo tiroOriginal
  | puedeSuperar obstaculo tiroOriginal = efectoLuegoDeSuperar obstaculo tiroOriginal
  | otherwise = tiroDetenido

tiroDetenido = unTiro 0 0 0 --No es superado, entonces la velocidad, precision y altura quedan en 0

tunelConRampita :: Obstaculo
tunelConRampita unTiro = unObstaculo superaTunelConRampita efectoTunelConRampita

superaTunelConRampita :: Tiro -> Bool
superaTunelConRampita tiro = precision tiro > 90 && vaAlRasDelSuelo tiro 

vaAlRasDelSuelo = (==0).altura

efectoTunelConRampita :: Tiro -> Tiro 
efectoTunelConRampita tiro = {velocidad = velocidad tiro * 2, precision = 100, altura = 0}


{-b. Una laguna es superada si la velocidad del tiro es mayor a 80 y tiene una altura de entre 1 y 5 metros. Luego de superar una 
laguna el tiro llega con la misma velocidad y precisión, pero una altura equivalente a la altura original dividida por el largo 
de la laguna. -}

laguna :: Obstaculo
laguna largo = unObstaculo superaLaguna (efectoLaguna largo)

superaLaguna :: Tiro -> Bool
superaLaguna tiro = velocidad tiro > 80 && (between 1 5 . altura) tiro

efectoLaguna :: Int -> Tiro -> Tiro 
efectoLaguna largo tiroOriginal = tiroOriginal {altura = altura tiroOriginal 'div' largo}


{-c. Un hoyo se supera si la velocidad del tiro está entre 5 y 20 m/s yendo al ras del suelo con una precisión mayor a 95. Al 
superar el hoyo, el tiro se detiene, quedando con todos sus componentes en 0.   -}

hoyo :: Obstaculo
hoyo unTiro = unObstaculo superaHoyo efectoHoyo

superaHoyo :: Tiro -> Bool
superaHoyo unTiro = (between 5 20.velocidad) tiro && vaAlRasDelSuelo tiro

efectoHoyo :: Tiro -> Tiro 
efectoHoyo _ = tiroDetenido



----PUNTO 4----

--a. Definir palosUtiles que dada una persona y un obstáculo, permita determinar qué palos le sirven para superarlo.

palosUtiles :: Jugador -> Obstaculo -> [Palo]
palosUtiles unJugador obstaculo = filter (leSirveParaSuperar unJugador obstaculo) palos --palos es una funcion creada anteriormente 

leSirveParaSuperar :: Jugador -> Obstaculo -> Palo -> Bool
leSirveParaSuperar unJugador obstaculo palo = puedeSuperar obstaculo (golpe unJugador palo) --golpe es una funcion creada anteriormente 

{-b. Saber, a partir de un conjunto de obstáculos y un tiro, cuántos obstáculos consecutivos se pueden superar.
Por ejemplo, para un tiro de velocidad = 10, precisión = 95 y altura = 0, y una lista con dos túneles con rampita seguidos 
de un hoyo, el resultado sería 2 ya que la velocidad al salir del segundo túnel es de 40, por ende no supera el hoyo. -}

cuantosObstaculosConsecutivosSupera :: Tiro -> [Obstaculo] -> Int
cuantosObstaculosConsecutivosSupera tiro [] = 0
cuantosObstaculosConsecutivosSupera tiro (obstaculo : obstaculos) 
  | puedeSuperar obstaculo tiro 
      = 1 + cuantosObstaculosConsecutivosSupera (efectoLuegoDeSuperar obstaculo tiro) obstaculos --El obstaculo que superamos + los siguientes que supere
  | otherwise = 0                                            

{-c.  Definir paloMasUtil que recibe una persona y una lista de obstáculos y determina cuál es el palo que le permite superar 
más obstáculos con un solo tiro. -}

--Usar las funciones: 
maximoSegun f = foldl1 (mayorSegun f)
mayorSegun f a b
  | f a > f b = a
  | otherwise = b

paloMasUtil :: Jugador -> [Obstaculo] -> Palo
paloMasUtil jugador obstaculos = maximoSegun (flip cuantosObstaculosConsecutivosSupera obstaculos.golpe jugador) palos


----PUNTO 5----

{-Dada una lista de tipo [(Jugador, Puntos)] que tiene la información de cuántos puntos ganó cada niño al finalizar el torneo, 
se pide retornar la lista de padres que pierden la apuesta por ser el “padre del niño que no ganó”. Se dice que un niño ganó 
el torneo si tiene más puntos que los otros niños.   -}

jugadorDeTorneo = fst
puntosGanados = snd

pierdenLaApuesta :: [(Jugador, Puntos)] -> [String]
pierdenLaApuesta puntosDeTorneo = (map (padre.jugadorDeTorneo) . filter (not . gano puntosDeTorneo)) puntosDeTorneo

gano :: [(Jugador, Puntos)] -> (Jugador, Puntos) -> Bool
gano puntosDeTorneo puntosDeUnJugador = (all ((< puntosGanados puntosDeUnJugador).puntosGanados) 
    . filter (/= puntosDeUnJugador)) puntosDeTorneo