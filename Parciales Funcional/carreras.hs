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









