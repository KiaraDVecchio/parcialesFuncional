import Text.Show.Functions ()

data Ninja = Ninja {
  nombre       :: String,
  herramientas :: [Herramienta],
  jutsus       :: [Jutsu],
  rango        :: Int
} deriving Show

type Herramienta = (String, Int)

bombas de humo
kunais
shurikens
sellos explosivos

mapHerramientas :: ([Herramienta] -> [Herramienta]) -> Ninja -> Ninja
mapHerramientas unaFuncion unNinja = unNinja { herramientas = unaFuncion . herramientas $ unNinja }

mapCantidad :: (Int -> Int) -> Herramienta -> Herramienta
mapCantidad unaFuncion (unNombre, unaCantidad) = (unNombre, unaFuncion unaCantidad)

nombreHerramienta :: Herramienta -> String
nombreHerramienta = fst

{-PARTE A-}

{- obtenerHerramienta: cada ninja debe poder obtener una cantidad específica de una herramienta en particular teniendo en cuenta que:
i. si la suma de todas sus herramientas más la cantidad a obtener es menor o igual a 100, puede hacerlo sin problemas;
ii. en caso contrario, obtiene la cantidad que pueda sin excederse de 100 herramientas.  -}

obtenerHerramienta :: Herramienta -> Ninja -> Ninja                         
obtenerHerramienta unaHerramienta unNinja = mapHerramientas ((mapCantidad . min . cuantasHerramientasPuedeObtener) unNinja unaHerramienta :) unNinja

cuantasHerramientasPuedeObtener :: Ninja -> Int  --Calcula cuántas herramientas adicionales puede obtener sin excederse de 100 herramientas.
cuantasHerramientasPuedeObtener = (100 -) . cantidadDeHerramientas  --Resta la cantidad total de herramientas de 100 para determinar cuántas herramientas adicionales puede obtener

cantidadDeHerramientas :: Ninja -> Int  -- Calcula la suma de todas las herramientas que posee un ninja.
cantidadDeHerramientas = sum . map snd . herramientas  --herramientas devuelve la lista de pares (Herramienta, Int), map snd extrae la cantidad de cada herramienta y sum suma todas las cantidades de herramientas.


{-b. usarHerramienta: cuando un ninja usa alguna de sus herramientas no mide cuántas utiliza, por lo que se queda sin 
ella y no debe figurar más entre sus pertenencias.  -}

usarHerramienta :: String -> Ninja -> Ninja  -- Elimina una herramienta específica del inventario de un ninja.
usarHerramienta unaHerramienta unNinja = mapHerramientas (filter ((/= unNombreDeHerramienta) . nombreHerramienta)) unNinja
--deja en la lista las herramientas que NO coinciden con la herramienta dada, /= significa ´distinto´


{-PARTE B-}

data Mision = Mision {
  cantidadDeNinjas :: Int,
  rangoRecomendado :: Int,
  ninjasEnemigos   :: [Ninja],
  recompensa       :: Herramienta
} deriving Show

type Equipo = [Ninja]

{-a. esDesafiante: dado un equipo de ninjas, una misión es desafiante cuando al menos alguien del equipo tiene menor rango 
que el recomendado y hay que derrotar al menos 2 enemigos. -}

esDesafiante :: Equipo -> Mision -> Bool
esDesafiante unEquipo unaMision = tieneMiembroNoCalificadoPara unEquipo unaMision && ((>= 2).length.ninjasEnemigos) unaMision

tieneMiembroNoCalificadoPara :: Equipo -> Mision -> Bool
tieneMiembroNoCalificadoPara unEquipo unaMision = any (not . estaCalificadoPara unaMision) unEquipo

estaCalificadoPara :: Mision -> Ninja -> Bool
estaCalificadoPara unaMision unNinja = rango unNinja >= rangoRecomendado unaMision


{-b. esCopada: esto pasa cuando la recompensa de la misión son 3 bombas de humo, 5 shurikens o 14 kunais.  -}

esCopada :: Mision -> Bool
esCopada unaMision = recompensa unaMision `elem` [("bombasDeHumo", 3), ("shurikens", 5), ("kunais", 14)]


{-c. esFactible: para que una misión sea factible no tiene que ser desafiante y además el grupo debe contar con la cantidad 
de ninjas necesaria o la suma total de herramientas del equipo debe ser superior a 500.  -}

esFactible :: Equipo -> Mision -> Bool
esFactible unEquipo unaMision = (not . esDesafiante unEquipo) unaMision && estaBienPreparadoPara unEquipo unaMision

estaBienPreparadoPara :: Equipo -> Mision -> Bool
estaBienPreparadoPara unEquipo unaMision = tieneSuficientesNinjasPara unEquipo unaMision || estanBienArmados unEquipo

tieneSuficientesNinjasPara :: Equipo -> Mision -> Bool
tieneSuficientesNinjasPara unEquipo unaMision = length unEquipo >= cantidadDeNinjas unaMision

estanBienArmados :: Equipo -> Bool
estanBienArmados = (> 500) . sum . map cantidadDeHerramientas


--Las misiones se pueden completar con éxito o no:
{- d. fallarMision: Cuando una misión falla sólo quedan en el equipo quienes tengan el rango recomendado o superior. -}
{- e. cumplirMision: si todo sale bien, se promociona de rango a cada miembro del equipo. Además obtendrán la recompensa 
teniendo en cuenta la restricción del máximo de herramientas.  -}

fallarMision :: Mision -> Equipo -> Equipo
fallarMision = filter . estaCalificadoPara

cumplirMision :: Mision -> Equipo -> Equipo  --Actualizar cada miembro del equipo promocionándolo de rango y añadiéndole la recompensa de la misión.
cumplirMision unaMision = map (obtenerHerramienta (recompensa unaMision) . promover)

promover :: Ninja -> Ninja
promover = mapRango succ   --succ devuelve el sucesor de un número (incrementa en 1).

--Jutsus
{-f. clonesDeSombra: reduce la cantidad de ninjas que se necesitan para una misión en el mismo número que los clones de sombra 
creados. ¡El tamaño del equipo no puede ser menor a 1!  -}

type Jutsu = Mision -> Mision

clonesDeSombra :: Int -> Jutsu
clonesDeSombra = mapCantidadDeNinjas . subtract

mapCantidadDeNinjas :: (Int -> Int) -> Mision -> Mision
mapCantidadDeNinjas unaFuncion unaMision = unaMision { cantidadDeNinjas = max 1 (unaFuncion (cantidadDeNinjas unaMision)) }

{-g. fuerzaDeUnCentenar: elimina a todos los enemigos con rango menor a 500. -}

fuerzaDeUnCentenar :: Jutsu
fuerzaDeUnCentenar unaMision = unaMision { ninjasEnemigos = filter (not . rangoMenor) (ninjasEnemigos unaMision) }

rangoMenor :: Ninja -> Bool
rangoMenor unNinja = rango unNinja < 500

{- Se pide modelar ejecutarMision. Cuando se ejecuta una misión, todos los ninjas del grupo usan todos sus 
jutsus en la misión. Luego, si la misión es copada o factible, se cumple. Caso contrario la misión se falla.  -}

ejecutarMision :: Equipo -> Mision -> Equipo
ejecutarMision unEquipo = completarMision unEquipo . usarTodosSusJutsus unEquipo

usarTodosSusJutsus :: Equipo -> Mision -> Mision
usarTodosSusJutsus unEquipo unaMision = foldr ($) unaMision . concatMap jutsus $ unEquipo

completarMision :: Equipo -> Mision -> Equipo
completarMision unEquipo unaMision
  | esCopada unaMision || esFactible unEquipo unaMision = cumplirMision unaMision unEquipo
  | otherwise                                           = fallarMision unaMision unEquipo


{- Existe la Gran Guerra Ninja, una misión de rango 100 que necesita al menos 100000 ninjas para completarse, 
tiene infinitos enemigos y su recompensa es el abanico de Madara Uchiha.
Se pide modelar la granGuerraNinja sabiendo, además, que tiene infinitos villanos y son Zetsu 1, Zetsu 2, 
Zetsu 3... Zetsu N, el rango de todos es de 600 y no tienen jutsus ni herramientas.   -}

granGuerraNinja :: Mision
granGuerraNinja = Mision {
  cantidadDeNinjas = 100000,
  rangoRecomendado = 100,
  ninjasEnemigos   = infinitosZetsus,
  recompensa       = ("Honor", 1)
}

infinitosZetsus :: [Ninja]
infinitosZetsus = map zetsu [1..]

zetsu :: Int -> Ninja
zetsu unNumero = Ninja {
  nombre       = "Zetsu " ++ show unNumero,
  rango        = 600,
  jutsus       = [],
  herramientas = []
}

{- Sabiendo esto y teniendo en cuenta un equipo de ninjas finitos, responder qué devuelve y por qué en las
siguientes funciones:
a. esDesafiante
b. esCopada
c. fuerzaDeUnCentenar 


Si la misión es copada, termina de ejecutar sin problemas y se cumple la misión.

Si el equipo es finito y la misión no es desafiante porque el equipo no tiene un miembro no calificado, termina 
sin problemas y se cumple la misión.

Si el equipo es finito, la misión no es desafiante porque el equipo no tiene un miembro no calificado, y no es 
factible porque el equipo no está bien preparado, termina sin problemas y se falla la misión.

En caso contrario, no termina de evaluar, ya sea porque tiene que evaluar la totalidad de la lista de enemigos, 
o la totalidad del equipo.
-}
