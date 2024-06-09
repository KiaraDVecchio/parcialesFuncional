import Text.Show.Functions ()
import Data.Char (toUpper)

data Barbaro = Barbaro {
    nombre :: String,
    fuerza :: Int,
    habilidiades :: [String],
    objetos :: [Objeto],
} deriving Show

type Objeto = Barbaro -> Barbaro

dave = Barbaro "Dave" 100 ["tejer","escribirPoesia"] [espadas 10 , varitasDefectuosas]
juan = Barbaro "Juan" 80 ["pintar"] [megafonoBarbarico, espadas 5]
faffy = Barbaro "Faffy" 90 ["robar", "gritoDeGuerra"] []
astro = Barbaro "Astro" 110 ["nadar", "gritoDeGuerra", "caligrafia"] []

aventura1 = Aventura "Aventura 1" [invasionDeSuciosDuendes]
aventura2 = Aventura "Aventura 2" [cremalleraDelTiempo, saqueo]
aventura3 = Aventura "Aventura 3" [gritoDeGuerra, caligrafia]

--accesors, funciones para mas adelante no repetir logica

-- Ejemplo, recibe una funcion y un Barbaro, y le aplica esa funcion al nombre del barbaro
mapNombre :: (String -> String) -> Barbaro -> Barbaro
mapNombre f unBarbaro = unBarbaro {nombre = f . nombre $ unBarbaro}

mapFuerza :: (Int -> Int) -> Barbaro -> Barbaro
mapFuerza f unBarbaro = unBarbaro {fuerza = f . fuerza $ unBarbaro}

mapHabilidades :: ([String] -> [String]) -> Barbaro -> Barbaro
mapHabilidades f unBarbaro = unBarbaro {habilidades = f . habilidades $ unBarbaro}

mapObjetos :: ([String] -> [String]) -> Barbaro -> Barbaro
mapObjetos f unBarbaro = unBarbaro {objetos = f . objetos $ unBarbaro}



------{-Punto 1-}------

--1) Las espadas aumentan la fuerza de los bárbaros en 2 unidades por cada kilogramo de peso.
espada :: Int -> Objeto
espada pesoEspada unBarbaro = mapFuerza (+ pesoEspada * 2) unBarbaro

--2)Los amuletosMisticos puerco-marranos otorgan una habilidad dada a un bárbaro.
--Tanto en amuletosMisticos como en varitasDefectuosas repito logica, extraigo para hacer una nueva llamada agregarHabilidad. NO es un objeto, solo es una funcion auxiliar

agregarHabilidad :: String -> Barbaro -> Barbaro
agregarHabilidad unaHabilidad unBarbaro = mapHabilidades (++ [unaHabilidad]) unBarbaro -- Agregamos un elemento (una habilidad) a la lista de sus habilidades

amuletosMisticos :: String -> Objeto
amuletosMisticos unaHabilidad unBarbaro = agregarHabilidad

--3)Las varitasDefectuosas, añaden la habilidad de hacer magia, pero desaparecen todos los demás objetos del bárbaro.
varitasDefectuosas :: Objeto
varitasDefectuosas unBarbaro = (agregarHabilidad "hacerMagia" . desaparecerObjetos) unaBarbaro

desaparecerObjetos :: Objeto
desaparecerObjetos unBarbaro = unBarbaro {objetos = [varitasDefectuosas]}

--4)Una ardilla, que no hace nada.
ardilla :: Objeto
ardilla = id       -- La funcion id toma un parametro y devuelve ese mismo parametro

--5)Una cuerda, que combina dos objetos distintos, obteniendo uno que realiza las transformaciones de los otros dos.
cuerda :: Objeto -> Objeto -> Objeto
cuerda unObjeto otroObjeto = unObjeto . otroObjeto

{- Otra forma de hacerlo:
cuerda :: Objeto -> Objeto -> Objeto
cuerda = (.)      
-}

------{-Punto 2-}------

--El megafono es un objeto que potencia al bárbaro, concatenando sus habilidades y poniéndolas en mayúsculas
megafono :: Objeto
megafono unBarbaro = mapHabilidades (map (map toUpper . concat)) unBarbaro -- toUpper que toma una Char y devuelve un Char pero en mayúscula

{- Explicacion del doble map:
Primer map: Aplica toUpper a cada carácter dentro de un String.
Segundo map: Aplica la función de transformación (en este caso, map toUpper) a cada String dentro de la 
lista de habilidades [String].-}

--Definir al objeto megafonoBarbarico, que está formado por una cuerda, una ardilla y un megáfono. 
megafonoBarbarico :: Objeto
megafonoBarbarico = cuerda ardilla megafono



------{-Punto 3-}------

type Aventura = [Evento]
type Evento = Barbaro -> Bool --Ya que los eventos definen si un barbaro sobrevive o no

--1)invasionDeSuciosDuendes: Un bárbaro sobrevive si sabe “Escribir Poesía Atroz”
invasionDeSuciosDuendes :: Evento
invasionDeSuciosDuendes barbaro = (elem "Escribir Poesia Atroz". habilidades) barbaro --escribir debe ser una de las habilidades


--2)cremalleraDelTiempo: Un bárbaro sobrevive si no tiene pulgares. Los llamados Faffy y Astro no tienen pulgares, los demás sí. 
cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = nombre unBarbaro `notElem` ["Faffy", "Astro"]

{-Otra forma: 
cremalleraDelTiempo :: Evento
cremalleraDelTiempo unBarbaro = nombre unBarbaro == "Faffy" || nombre unBarbaro == "Astro "  -}


--3)ritualDeFechorias: Un bárbaro puede sobrevivir si pasa una o más pruebas como las siguientes: 

--a. saqueo: El bárbaro debe tener la habilidad de robar y tener más de 80 de fuerza.
saqueo :: Evento
saqueo unBarbaro = "robar" `elem` habilidades unBarbaro && ((>80).fuerza) unBarbaro

--b. gritoDeGuerra: El bárbaro debe tener un poder de grito de guerra igual a la cantidad de letras de sus 
--habilidades. El poder necesario para aprobar es 4 veces la cantidad de objetos del bárbaro.
gritoDeGuerra :: Evento
gritoDeGuerra unBarbaro = poderDeGritoDeGuerra unBarbaro >= cantidadDeLetrasDeHabilidades

poderDeGritoDeGuerra :: Barbaro -> Int
poderDeGritoDeGuerra = (*4).lenght.objetos

cantidadDeLetrasDeHabilidades :: Barbaro -> Int
cantidadDeLetrasDeHabilidades unBarbaro = length (concat (habilidades unBarbaro)

--c. caligrafia: El bárbaro tiene caligrafía perfecta si sus habilidades contienen más de 3 vocales y comienzan con mayúscula.
caligrafia :: Evento
caligrafia unBarbaro = (length (filter (`elem` "aeiouAEIOU") (concat (habilidades unBarbaro))) > 3) && 
        (isUpper (head (nombre unBarbaro)))

{-Explicación: 
--isUpper toma un carácter (Char) como entrada y devuelve True si el carácter es una letra mayúscula y False 
en caso contrario. -}

ritualDeFechorias :: [Evento] -> Evento
ritualDeFechorias eventos unBarbaro = pasaUnaAventura any unBarbaro eventos --pasaUnaAventura recibe el criterio (any), unBarbaro y unaAventura (evento)

--Definir la función sobrevivientes que tome una lista de bárbaros y una aventura, y diga cuáles bárbaros la sobreviven (es decir, pasan todas las pruebas)
sobrevivientes :: [Barbaro] -> Aventura -> [Barbaro]
sobrevivientes unosBarbaros unaAventura = filter (\unBarbaro -> pasaUnaAventura all unBarbaro unaAventura) unosBarbaros

--Extraemos la logica de ritualDeFechorias y de sobrevivientes
pasaUnaAventura :: ...
pasaUnaAventura criterio unBarbaro unaAventura = criterio (\evento -> evento unBarbaro) unaAventura

{-Explicación de ritualDeFechorias: 
--Si aplicamos cada uno de los eventos al barbaro, decimos si cumple o no cumple, y con que alguno cumpla con
el any, ya pasaria el ritual de fechorias -}

--Ejemplo de una aventura
aventuraEjemplo :: Aventura
aventuraEjemplo = [cremalleraDelTiempo, gritoDeGuerra, caligrafia]


------{-Punto 4-}------

{-a) Los bárbaros se marean cuando tienen varias habilidades iguales. Por todo esto, nos piden desarrollar una 
función que elimine los elementos repetidos de una lista (sin utilizar nub ni nubBy). Puede usarse recursividad. -}

sinRepetidos :: [String] -> [String]
sinRepetidos [] = []  --Al ser una lista, el caso base va a ser el patron de lista vacia
sinRepetidos (cabeza : cola) 
    | elem cabeza cola = sinRepetidos cola 
    | otherwise = (cabeza : sinRepetidos cola)  

--Exp: si el elemento cabeza está en la cola, devuelve solo la cola (elimino la cabeza, el elemento repetido),
--sino, devuelvo la misma funcion pero analizando ahora la cola


{-b El descendiente de un bárbaro comparte su nombre, y un asterisco por cada generación. Por ejemplo "Dave*", "Dave**" , "Dave***" , etc. 
Además, tienen su mismo poder, habilidades sin repetidos, y los objetos de su padre, pero antes de pasar 
a la siguiente generación, utilizan (aplican sobre sí mismos) los objetos. Por ejemplo, el hijo de Dave será equivalente a:
(ardilla.varitasDefectuosas) (Barbaro "Dave*" 100 ["tejer","escribirPoesia"] [ardilla, varitasDefectuosas])
Definir la función descendientes, que dado un bárbaro nos de sus infinitos descendientes. -}

descendiente :: Barbaro -> Barbaro
descendiente unBarbaro = utilizarObjetos.mapNombre (++ "*"). mapHabilidades sinRepetidos 

utilizarObjetos :: Barbaro -> Barbaro 
utilizarObjetos unBarbaro = foldr ($) unBarbaro (objetos unBarbaro) --Toma un Barbaro y le aplica todos los objetos que posee.

{-Explicacion: 
foldr toma el último objeto en la lista objetos unBarbaro y lo aplica a unBarbaro (lo aplica xq usamos $, que es 
el operador de aplicacion. Luego, toma el penúltimo objeto y lo aplica al resultado del paso anterior.
Continúa aplicando cada objeto desde la derecha hacia la izquierda hasta que todos los objetos se han aplicado.-}

descendientes :: Barbaro -> [Barbaro]
descendientes unBarbaro = iterate descendiente unBarbaro
--iterate: agarra una funcion y un elemento y aplica iterativamente esa funcion al ultimo elemento

--Otra forma: 

-- Función que genera el nombre del descendiente
generarNombreDescendiente :: Barbaro -> Int -> String
generarNombreDescendiente unBarbaro generacion = nombre unBarbaro ++ replicate generacion '*'

-- Función que genera el descendiente de una generación dada
generarDescendiente :: Barbaro -> Int -> Barbaro
generarDescendiente unBarbaro generacion = aplicarObjetos (Barbaro nuevoNombre (fuerza unBarbaro) (mapHabilidades sinRepetidos) (objetos unBarbaro))
  where
    nuevoNombre = generarNombreDescendiente unBarbaro generacion


--C.¿Se podría aplicar sinRepetidos sobre la lista de objetos? ¿Y sobre el nombre de un bárbaro? ¿Por qué?

--No se puede aplicar sinRepetidos xq los objetos del barbaro son funciones, y las funciones NO son comparables
--Sobre el nombre del barbaro si xq es un String
