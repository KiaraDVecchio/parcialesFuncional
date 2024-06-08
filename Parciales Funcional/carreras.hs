import Text.Show.Functions

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distanciaRecorrida :: Int
} deriving (Show, Eq)

type Carrera = [Auto]


------{-Punto 1-}------

{-a. Saber si un auto está cerca de otro auto, que se cumple si son autos distintos y la distancia que hay 
entre ellos (en valor absoluto) es menor a 10.   -}

estaCerca :: Auto -> Auto -> Bool
estaCerca unAuto otroAuto = unAuto /= otroAuto && distanciaEntre unAuto otroAuto < 10

distanciaEntre :: Auto -> Auto -> Number 
distanciaEntre unAuto otroAuto = abs . (distanciaRecorrida unAuto -) . distanciaRecorrida otroAuto


{-b. Saber si un auto va tranquilo en una carrera, que se cumple si no tiene ningún auto cerca y les va 
ganando a todos (por haber recorrido más distancia que los otros).   -}

autoVaTranquilo :: Auto -> Carrera -> Bool 
autoVaTranquilo unAuto unaCarrera = (not.tieneAlgunAutoCerca unAuto) unaCarrera && vaGanando unAuto unaCarrera

tieneAlgunAutoCerca :: Auto -> Carrera -> Bool 
tieneAlgunAutoCerca unAuto = any (estaCerca unAuto) 

vaGanando :: Auto -> Carrera -> Bool
vaGanando auto  = all (leVaGanando auto) . filter (/= auto)
--Un auto está ganando si le va ganando a todos los otros autos en la carrera (all (leVaGanando auto)) excluyendo el auto mismo (filter (/= auto)).

leVaGanando :: Auto -> Auto -> Bool
leVaGanando ganador = (< distanciaRecorrida ganador).distanciaRecorrida


{-c. Conocer en qué puesto está un auto en una carrera, que es 1 + la cantidad de autos de la carrera que 
le van ganando.  -}

puestoDeUnAuto :: Auto -> Carrera -> Int
puestoDeUnAuto auto = (1 +) . length . filter (not . leVaGanando auto) 



------{-Punto 2-}------

{-a. Hacer que un auto corra durante un determinado tiempo. Luego de correr la cantidad de tiempo indicada, 
la distancia recorrida por el auto debería ser equivalente a la distancia que llevaba recorrida + ese tiempo 
* la velocidad a la que estaba yendo.   -}

correrDuranteUnTiempo :: Int -> Auto -> Auto 
correrDuranteUnTiempo tiempoCorrida auto = 
    auto { distanciaRecorrida = distanciaRecorrida auto + tiempoCorrida * velocidad auto } 


{-b.ii) A partir de un modificador de tipo Int -> Int, queremos poder alterar la velocidad de un auto de modo 
que su velocidad final sea la resultante de usar dicho modificador con su velocidad actual.  -}

type ModificadorDeVelocidad = Int -> Int 

alterarVelocidad :: ModificadorDeVelocidad -> Auto -> Auto
alterarVelocidad modificador auto = auto { velocidad = (modificador . velocidad) auto}


{- ii) Usar la función del punto anterior para bajar la velocidad de un auto en una cantidad indicada de modo 
que se le reste a la velocidad actual la cantidad indicada, y como mínimo quede en 0, ya que no es válido que 
un auto quede con velocidad negativa.   -}

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad velocidadABajar = alterarVelocidad (max 0 . subtract velocidadABajar)



------{-Punto 3-}------

{- Nota: disponemos de una función que puede ser de utilidad para manipular el estado de la carrera. -}

afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista
  = (map efecto . filter criterio) lista ++ filter (not.criterio) lista

{- Inicialmente queremos poder representar los siguientes power ups, pero debería ser fácil incorporar más 
power ups a futuro para enriquecer nuestro programa:
a. terremoto: luego de usar este poder, los autos que están cerca del que gatilló el power up bajan su 
velocidad en 50.  -}  

type PowerUp = Auto -> Carrera -> Carrera

terremoto :: PowerUp
terremoto autoQueGatillo = afectarALosQueCumplen (estaCerca autoQueGatillo) (bajarVelocidad 50)


{-b. miguelitos: este poder debe permitir configurarse con una cantidad que indica en cuánto deberán bajar la 
velocidad los autos que se vean afectados por su uso. Los autos a afectar son aquellos a los cuales el auto 
que gatilló el power up les vaya ganando.   -}

miguelitos :: Int -> PowerUp
miguelitos velocidadABajar autoQueGatillo =
     afectarALosQueCumplen (leVaGanando autoQueGatillo) (bajarVelocidad velocidadABajar)


{-c. jet pack: este poder debe afectar, dentro de la carrera, solamente al auto que gatilló el poder. El jet 
pack tiene un impacto que dura una cantidad limitada de tiempo, el cual se espera poder configurar.
Cuando se activa el poder del jet pack, el auto afectado duplica su velocidad actual, luego corre durante el 
tiempo indicado y finalmente su velocidad vuelve al valor que tenía antes de que se active el poder.
Por simplicidad, no se espera que los demás autos que participan de la carrera también avancen en ese tiempo. -}

jetPack :: Int -> PowerUp
jetPack tiempo autoQueGatillo = afectarALosQueCumplen (== autoQueGatillo)
        (alterarVelocidad (\ _ -> velocidad autoQueGatillo) . correrDuranteUnTiempo tiempo . alterarVelocidad (*2))

{-Al aplicarse alterarVelocidad (\_ -> velocidad autoQueGatillo), se restaura la velocidad del auto 
a su valor original.   -}


------{-Punto 3-}------

{-Queremos simular una carrera, para lo cual se provee una lista de eventos, que son funciones que permiten ir de un 
estado de la carrera al siguiente, y el estado inicial de la carrera a partir del cual se producen dichos eventos. 
Con esta información buscamos generar una “tabla de posiciones”, que incluye la información de en qué puesto quedó cada 
auto asociado al color del auto en cuestión.  -}

{-a. Desarrollar la función simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int, Color)] que permita obtener la 
tabla de posiciones a partir del estado final de la carrera, el cual se obtiene produciendo cada evento uno detrás del 
otro, partiendo del estado de la carrera recibido.    -}

type Eventos = Carrera -> Carrera 
type Color = String 

simularCarrera :: Carrera -> [Evento] -> [(Int, Color)]
simularCarrera carrera eventos = (tablaDePosiciones . procesarEventos eventos) carrera --"carrera" es el estado final de la carrera

tablaDePosiciones :: Carrera -> [(Int, Color)]
tablaDePosiciones carrera = map (entradaDeTabla carrera) carrera

entradaDeTabla :: Carrera -> Auto -> (Int, String)  --Devuelve una tupla con el puesto del auto y su color.
entradaDeTabla carrera auto = (puesto auto carrera, color auto) 

procesarEventos :: [Evento] -> Carrera -> Carrera  --Devuelve el estado final de la carrera tras aplicar todos los eventos.
procesarEventos eventos carreraInicial = foldl (\carreraActual evento -> evento carreraActual)  carreraInicial eventos  


{-b.i) correnTodos que hace que todos los autos que están participando de la carrera corran durante un tiempo indicado. -}

correnTodos :: Number -> Evento
correnTodos tiempo = map (correr tiempo)

{-b.ii) usaPowerUp que a partir de un power up y del color del auto que gatilló el poder en cuestión, encuentre el auto 
correspondiente dentro del estado actual de la carrera para usarlo y produzca los efectos esperados para ese power up. -}

usaPowerUp :: PowerUp -> Color -> Evento
usaPowerUp powerUp colorBuscado carrera = powerUp autoQueGatillaElPoder carrera
    where autoQueGatillaElPoder = find ((== colorBuscado).color) carrera  --busca el auto en la carrera cuyo color coincide con colorBuscado.

find :: (c -> Bool) -> [c] -> c --find toma un predicado ((== colorBuscado) . color)) y una lista, y devuelve el primer elemento de la lista que satisface el predicado.
find cond = head . filter cond

{-c. Mostrar un ejemplo de uso de la función simularCarrera con autos de colores rojo, blanco, azul y negro que vayan 
inicialmente a velocidad 120 y su distancia recorrida sea 0, de modo que ocurran los siguientes eventos:
- todos los autos corren durante 30 segundos
- el azul usa el power up de jet pack por 3 segundos
- el blanco usa el power up de terremoto
- todos los autos corren durante 40 segundos
- el blanco usa el power up de miguelitos que reducen la velocidad en 20
- el negro usa el power up de jet pack por 6 segundos
- todos los autos corren durante 10 segundos  -}

ejemploDeUsoSimularCarrera =
    simularCarrera autosDeEjemplo [
        correnTodos 30,
        usaPowerUp (jetPack 3) "azul",
        usaPowerUp terremoto "blanco",
        correnTodos 40,
        usaPowerUp (miguelitos 20) "blanco",
        usaPowerUp (jetPack 6) "negro",
        correnTodos 10
    ]

autosDeEjemplo :: [Auto]
autosDeEjemplo = map (\color -> Auto color 120 0) ["rojo", "blanco", "azul", "negro"]


{-a. Si se quisiera agregar un nuevo power up, un misil teledirigido, que para poder activarlo se deba indicar el 
color del auto al que se quiere impactar, ¿la solución actual lo permite o sería necesario cambiar algo de lo 
desarrollado en los puntos anteriores? Justificar.   -}

Se puede agregar sin problemas como una función más misilTeledirigido :: Color -> PowerUp, y usarlo como:
usaPowerUp (misilTeledirigido "rojo") "azul" :: Evento

{-b. Si una carrera se conformara por infinitos autos, ¿sería posible usar las funciones del punto 1b y 1c de modo 
que terminen de evaluarse? Justificar.  --}

vaTranquilo puede terminar sólo si el auto indicado no va tranquilo (en este caso por tener a alguien cerca, si las 
condiciones estuvieran al revés, terminaría si se encuentra alguno al que no le gana).
Esto es gracias a la evaluación perezosa, any es capaz de retornar True si se encuentra alguno que cumpla la condición 
indicada, y all es capaz de retornar False si alguno no cumple la condición correspondiente. Sin embargo, no podría 
terminar si se tratara de un auto que va tranquilo.

puesto no puede terminar nunca porque hace falta saber cuántos le van ganando, entonces por más que se pueda tratar de 
filtrar el conjunto de autos, nunca se llegaría al final para calcular la longitud.







