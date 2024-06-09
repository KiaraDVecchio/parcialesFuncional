
import Text.Show.Functions

data Animal = Animal {
    coeficiente :: Int,
    especie :: String, 
    capacidades :: [String]
} deriving Show

pinky :: Animal 
pinky = Animal {
    coeficiente = 60,
    especie     = "raton",
    capacidades = ["correr", "nadar", "hablar","hacer narf", "hacer asdf", "hacer carl"]
}

cerebro :: Animal
cerebro = Animal {
    coeficiente = 200,
    especie     = "raton",
    capacidades = ["pensar","construir"]
}

pepito :: Animal
pepito = Animal {
    coeficiente = 17,
    especie     = "raton",
    capacidades = ["destruenglonir el mundo", "hacer planes desalmados"]
}


{-2. Transformar a un animal de laboratorio:
a) inteligenciaSuperior: Incrementa en n unidades su coeficiente intelectual  -}

type Transformar = Animal -> Animal

inteligenciaSuperior :: Int -> Transformar
inteligenciaSuperior n unAnimal = cambiarCoeficiente (+n) unAnimal

cambiarCoeficiente :: (Int -> Int) -> Transformar
cambiarCoeficiente funcion unAnimal = unAnimal { coeficiente = funcion . coeficiente $ unAnimal }


--b) pinkificar: quitarle todas las habilidades que tenía

pinkificar :: Transformar
pinkificar animal = cambiarCapacidades quitarTodasLasHabilidades animal

quitarTodasLasHabilidades :: [String] -> [String]
quitarTodasLasHabilidades capacidades = []

cambiarCapacidades :: ([String] -> [String]) -> Transformar
cambiarCapacidades funcion unAnimal = unAnimal { capacidades = funcion . capacidades $ unAnimal }

{-Otra forma: 
pinkificar :: Transformar
pinkificar unAnimal = unAnimal { capacidades = [] }        -}


{-c) superpoderes:  le da habilidades nuevas	
En caso de ser un elefante: le da la habilidad “no tenerle miedo a los ratones”
En caso de ser un ratón con coeficiente intelectual mayor a 100: le agrega la habilidad de “hablar”. 
Si no, lo deja como está. -}

superpoderes :: Transformar
superpoderes unAnimal 
    | especie unAnimal == "Elefante" = agregarCapacidad "No tenerle miedo a los ratones" unAnimal
    | especie unAnimal == "raton" && coeficiente unAnimal > 100 = agregarCapacidad "hablar" unAnimal
    | otherwise = unAnimal 

cambiarCapacidad :: ([String] -> [String]) -> Animal -> Animal
cambiarCapacidad funcion unAnimal = unAnimal { capacidades = funcion . capacidad $ unAnimal }

agregarCapacidad :: String -> Animal -> Animal
agregarCapacidad unaCapacidad unAnimal = cambiarCapacidad (++ [unaCapacidad]) unAnimal


{- 3. Los científicos muchas veces desean saber si un animal cumple ciertas propiedades, porque luego las 
usan como criterio de éxito de una transformación. Desarrollar los siguientes criterios:
a. antropomórfico: si tiene la habilidad de hablar y su coeficiente es mayor a 60.     -}

type Criterio = Animal -> Bool

antropomorfico :: Criterio
antropomorfico animal = tieneCapacidadDe "Hablar" animal && coeficiente animal > 60

tieneCapacidadDe :: String -> Animal -> Bool
tieneCapacidadDe unaCapacidad unAnimal = elem unaCapacidad (capacidades unAnimal)


{-b. noTanCuerdo: si tiene más de dos habilidades de hacer sonidos pinkiescos. Hacer una función pinkiesco, 
que significa que la habilidad empieza con “hacer ”, y luego va seguido de una palabra "pinkiesca", es decir, 
con 4 letras o menos y al menos una vocal. Ejemplo:   -}

tieneVocal :: Char -> Bool
tieneVocal caracter = caracter `elem` "aeiouAEIOU"

tieneCuatroLetrasOMenos :: String -> Bool
tieneCuatroLetrasOMenos palabra = length palabra <= 4

esPalabraPinkinesca :: String -> Bool
esPalabraPinkinesca palabra = tieneCuatroLetrasOMenos palabra && any tieneVocal palabra

pinkinesco :: String -> Bool
pinkinesco palabra = take 6 palabra == "hacer " && esPalabraPinkinesca (drop 6 palabra)

filtrarPinkinesco :: [String] -> [String]
filtrarPinkinesco capacidades = filter pinkinesco capacidades  --Filtro las capacidades q sean pinkinescas

noTanCuerdo :: Criterio
noTanCuerdo animal = length (filtrarPinkinesco $ capacidades animal) > 2


{-4) Los científicos construyen experimentos: un experimento se compone de un conjunto de transformaciones 
sobre un animal, y un criterio de éxito. Se pide:
a. Modelar a los experimentos: dar un sinónimo de tipo. (hecho en el c.)       -}

type Experimento = ([Transformar], Criterio)

{-b. Desarollar experimentoExitoso: Dado un experimento y un animal, indica si al aplicar sucesivamente todas 
las transformaciones se cumple el criterio de éxito. -}

ejecutarExperimento :: Experimento -> Animal -> Animal  --Aplico las transformaciones al animal
ejecutarExperimento (transformaciones,criterio) animal = foldr ($) animal transformaciones

experimentoExitoso :: Experimento -> Animal -> Bool --Una vez aplicadas las transf, me fijo si cumple con el criterio 
experimentoExitoso (transformaciones,criterio) animal = criterio (ejecutarExperimento (transformaciones,criterio) animal)



{-c. Dar un ejemplo de consulta para representar la siguiente situación:
"En un ratón de coeficiente intelectual 17, con habilidades de destruenglonir el mundo y hacer planes 
desalmados, hacer un experimento que consista en pinkificarlo, luego darle inteligencia superior de 10 y por 
último darle superpoderes. Como criterio de éxito, ver si quedó antropomórfico" -}

transformacionesParaPepito :: [Transformar]
transformacionesParaPepito = [pinkificar, inteligenciaSuperior 10, superpoderes]

criterioDeExitoParaPepito :: Criterio
criterioDeExitoParaPepito = antropomorfico

experimentoPepito :: Experimento 
experimentoPepito = (transformacionesParaPepito, criterioDeExitoParaPepito)



{-5) Desarrollar los siguientes reportes, que a partir de una lista de animales, una lista de capacidades y un 
experimento (o una serie de transformaciones) permitan obtener:
a. una lista con los coeficientes intelectuales de los animales que entre sus capacidades, luego de efectuar el 
experimento, tengan alguna de las capacidades dadas.    -}

type Reporte = [Animal] -> [String] -> Experimento

reporteCoeficientes :: Reporte -> [Int]
reporteCoeficientes animales capacidadesDadas experimento = 
    map coeficiente (filter (tieneAlgunaCapacidad capacidadesDadas . ejecutarExperimento experimento) animales)

tieneAlgunaCapacidad :: [String] -> Animal -> Bool
tieneAlgunaCapacidad capacidadesDadas animal = any (`elem` capacidades animal) capacidadesDadas


{-b. una lista con las especie de los animales que, luego de efectuar el experimento, tengan entre sus capacidades 
todas las capacidades dadas.     -}

tieneTodasLasCapacidades :: [String] -> Animal -> Bool
tieneTodasLasCapacidades capacidadesDadas animal = all (`elem` capacidades animal) capacidadesDadas

especiesConTodasLasCapacidades :: Reporte -> [String]
especiesConTodasLasCapacidades animales capacidadesDadas experimento = 
    map especie (filter (tieneTodasLasCapacidades capacidadesDadas . aplicarExperimento experimento) animales)


{-c. una lista con la cantidad de capacidades de todos los animales que, luego de efectuar el experimento, 
no tengan ninguna de las capacidades dadas.  -} 

noTieneNingunaCapacidad :: [String] -> Animal -> Bool
noTieneNingunaCapacidad capacidadesDadas animal = all (`notElem` capacidades animal) capacidadesDadas

cantidadCapacidadesSinNingunaDada :: Reporte -> [Int]
cantidadCapacidadesSinNingunaDada animales capacidadesDadas experimento = 
    map (length . capacidades) (filter (noTieneNingunaCapacidad capacidadesDadas . aplicarExperimento experimento) animales)


--Funcion que extrae la lógica de las tres funciones: 

filtrarYTransformar :: ([String] -> Animal -> Bool) -> (Animal -> b) -> Reporte b
filtrarYTransformar criterio transformacion animales capacidadesDadas experimento =
    map transformacion (filter (criterio capacidadesDadas . aplicarExperimento experimento) animales)



{- 6. Aparece un nuevo animal que tiene infinitas capacidades. Dar ejemplos de experimentos que se puedan 
realizar y que no, si los hubiera. Justificar conceptualmente. -}

El problema principal con un animal que tiene infinitas capacidades radica en las operaciones que requieren 
recorrer o procesar completamente esta lista. En Haskell, listas infinitas son manejables solo cuando se puede 
trabajar con una parte finita de ellas, como acceder a los primeros elementos o aplicar transformaciones que no 
requieren consumir toda la lista.

Experimentos que se PUEDEN realizar: 
- inteligenciaSuperior: las transformaciones no dependen de la lista de capacidades del animal y, por lo tanto, 
se pueden aplicar sin problemas.
- superpoderes: Transformaciones que agregan una capacidad específica a la lista, sin necesidad de recorrerla 
completamente, pueden realizarse.
- antropomorfico: Criterios que no necesitan evaluar todas las capacidades del animal también son viables. 
Por ejemplo, si un criterio depende solo del coeficiente intelectual, la especie, o una capacidad específica
que puede ser verificada rápidamente.

Experimentos que NO SE PUEDEN realizar: 
- pinkificar: transformaciones que requieren recorrer toda la lista de capacidades para quitarlas, no pueden 
aplicarse a un animal con una lista infinita de capacidades, ya que esto llevaría a un ciclo infinito.
- noTanCuerdo: Criterios que requieren verificar cada capacidad en la lista tampoco pueden aplicarse, ya que 
intentar verificar una lista infinita de capacidades resulta en un ciclo infinito.
- cambiarCapacidades: Cualquier transformación que necesita condicionalmente verificar todas las capacidades 
antes de realizar un cambio no puede completarse con una lista infinita.


{- 7. Generar todas las posibles palabras pinkiescas. Pistas:
generateWordsUpTo, que toma una longitud y genera una lista con todas las posibles palabras de hasta la longitud dada.
generateWords que toma una longitud y genera una lista de palabras donde todas tienen exactamente la longitud dada.  -}

esPalabraPinkinesca :: String -> Bool
esPalabraPinkinesca palabra = length palabra <= 4 && any (`elem` "aeiou") palabra

-- Genera todas las palabras pinkiescas hasta una longitud dada
generatePinkiescas :: Int -> [String]
generatePinkiescas n = filter esPalabraPinkinesca (generateWordsUpTo n)
