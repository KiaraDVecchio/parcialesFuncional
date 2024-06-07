import Text.Show.Functions

data Turistas = Turista { 
    cansancio :: Int,
    stress :: Int,
    solitario :: Bool,
    idiomas :: [Idioma]
}  deriving Show

type Idioma = String
type Excursion = Tursita -> Turista

--Funciones auxiliares
cambiarStress :: Int -> Excursion
cambiarStress valor unTurista = unTurista { stress = stress unTurista + valor }

cambiarCansancio :: Int -> Excursion
cambiarCansancio valor unTurista = unTurista { cansancio = cansancio unTurista + valor }


--EXCURSIONES

--Ir a la playa: si está viajando solo baja el cansancio en 5 unidades, si no baja el stress 1 unidad.

irALaPlaya :: Excursion
irALaPlaya unTurista 
    | solitario unTurista = cambiarCansancio (-5) unTurista
    | otherwise           = cambiarStress (-1) unTurista

--Apreciar algún elemento del paisaje: reduce el stress en la cantidad de letras de lo que se aprecia. 

apreciarElementoDelPaisaje :: String -> Excursion 
apreciarElementoDelPaisaje elemento = cambiarStress (letrasDelElemento elemento)

letrasDelElemento :: String -> Int
letrasDelElemento elemento = -length elemento

--Salir a hablar un idioma específico: el turista termina aprendiendo dicho idioma y continúa el viaje acompañado.

idiomaEspecifico :: Idioma -> Excursion
idiomaEspecifico unIdioma = acompaniado . aprenderIdioma unIdioma

aprenderIdioma :: Idioma -> Excursion
aprenderIdioma unIdioma unTurista = unTurista  { idiomas = unIdioma : idiomas unTurista }

acompaniado :: Excursion
acompaniado unTurista = unTurista { solitario = False }

--Caminar ciertos minutos: aumenta el cansancio pero reduce el stress según la intensidad de la caminada, ambos en la misma 
--cantidad. El nivel de intensidad se calcula en 1 unidad cada 4 minutos que se caminen.

caminarMinutos :: Int -> Excursion
caminarMinutos minutos = cambiarCansancio (intensidad minutos) . cambiarStress (-intensidad minutos) 

intensidad :: Int -> Int
intensidad minutos = div minutos 4  

{-Paseo en barco: depende de cómo esté la marea
    -si está fuerte, aumenta el stress en 6 unidades y el cansancio en 10.
    -si está moderada, no pasa nada.
    -si está tranquila, el turista camina 10’ por la cubierta, aprecia la vista del “mar”, y sale a hablar con los tripulantes alemanes.-}

data Marea = Tranquila
           | Moderada
           | Fuerte

paseoEnBarco :: Marea -> Excursion
paseoEnBarco Tranquila = caminarMinutos 10 . apreciarElementoDelPaisaje "mar" . acompaniado 
paseoEnBarco Moderada = id
paseoEnBarco Fuerte = cambiarCansancio 10 . cambiarStress 6

--DEFINCION TURISTAS

ana :: Turista
ana = Turista { cansancio = 0 , stress = 20, solitario = False, idiomas = ["espaniol"] }

beto :: Turista
beto = Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman"] }

cathi :: Turista
cathi = Turista { cansancio = 15, stress = 15, solitario = True, idiomas = ["aleman", "catalan"] }


---- PUNTO 2 ----

{-a) Hacer que un turista haga una excursión. Al hacer una excursión, el turista además de sufrir los efectos propios de la 
excursión, reduce en un 10% su stress.  -}

hacerExcursion :: Excursion -> Turista -> Turista
hacerExcursion excursion = cambiarStressPorcentual (-10) . excursion

cambiarStressPorcentual :: Int -> Turista -> Turista 
cambiarStressPorcentual porcentaje unTurista = cambiarStress (div (porcentaje * stress unTurista) 100) unTurista --aplico regla de tres
                                              
--b) Dada la función
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2
{-Definir la función deltaExcursionSegun que a partir de un índice, un turista y una excursión determine cuánto 
varió dicho índice después de que el turista haya hecho la excursión. Llamamos índice a cualquier función que 
devuelva un número a partir de un turista.   -}

deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int 
deltaExcursionSegun indice unTurista excursion =  deltaSegun indice (hacerExcursion excursion unTurista) unTurista --deltaSegun resta el indice menos como quedo el turista luego de hacer la excursion

{-c) Usar la función anterior para resolver cada uno de estos puntos:
c.1) Saber si una excursión es educativa para un turista, que implica que termina aprendiendo algún idioma. -}

excursionEducativa :: Turista -> Excursion -> Bool
excursionEducativa unTurista = (> 0) . deltaExcursionSegun (length . idiomas) unTurista 

--c.2) Conocer las excursiones desestresantes para un turista. Estas son aquellas que le reducen al menos 3 unidades de stress al turista.

excursionesDesestresantes :: Turista -> [Excursion] -> [Excursion]
excursionesDesestresantes turista = filter (esDesestresante turista)

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista = (<= -3) . deltaExcursionSegun stress turista


---- PUNTO 3 ----

type Tour = [Excursion]

{-a) Completo: Comienza con una caminata de 20 minutos para apreciar una "cascada", luego se camina 40 minutos hasta una 
playa, y finaliza con una salida con gente local que habla "melmacquiano".  -}

completo :: Tour
completo = [caminarMinutos 20, apreciarElementoDelPaisaje "cascada", caminarMinutos 40, salidaLocal ] --Defino LISTA DE EXCURSIONES

salidaLocal :: Excursion
salidaLocal = aprenderIdioma "melmacquiano"


{-b) Lado B: Este tour consiste en ir al otro lado de la isla a hacer alguna excursión (de las existentes) que elija el turista. 
Primero se hace un paseo en barco por aguas tranquilas (cercanas a la costa) hasta la otra punta de la isla, luego realiza la 
excursión elegida y finalmente vuelve caminando hasta la otra punta, tardando 2 horas. -}

ladoB :: Excursion -> Tour
ladoB excursion = [ paseoEnBarco Tranquila, excursion, caminarMinutos 120 ]

{-c) Isla Vecina: Se navega hacia una isla vecina para hacer una excursión. Esta excursión depende de cómo esté la marea al 
llegar a la otra isla: si está fuerte se aprecia un "lago", sino se va a una playa. En resumen, este tour implica hacer un 
paseo en barco hasta la isla vecina, luego llevar a cabo dicha excursión, y finalmente volver a hacer un paseo en barco de 
regreso. La marea es la misma en todo el camino.   -}

islaVecina :: Marea -> Tour
islaVecina mareaVecina = [paseoEnBarco mareaVecina, excursionEnIslaVecina mareaVecina, paseoEnBarco mareaVecina]

excursionEnIslaVecina :: Marea -> Excursion
excursionEnIslaVecina Fuerte = apreciarElementoDelPaisaje "lago"
excursionEnIslaVecina _  = irALaPlaya


{-Modelar los tours para:
1) Hacer que un turista haga un tour. Esto implica, primero un aumento del stress en tantas unidades como cantidad de 
excursiones tenga el tour, y luego realizar las excursiones en orden. -}

hacerTour :: Turista -> Tour -> Turista
hacerTour unTurista tour =
  foldl (flip hacerExcursion) (cambiarStress (length tour) unTurista) tour

{-Explicacion: Utilizamos foldl para aplicar cada excursión del tour secuencialmente al turista. foldl (flip hacerExcursion) 
permite aplicar hacerExcursion en el orden correcto, comenzando con el turista con el stress incrementado. -}


{-2) Dado un conjunto de tours, saber si existe alguno que sea convincente para un turista. Esto significa que el tour tiene 
alguna excursión desestresante la cual, además, deja al turista acompañado luego de realizarla.  -}

propuestaConvincente :: Turista -> [Tour] -> Bool
propuestaConvincente turista = any (esConvincente turista)

esConvincente :: Turista -> Tour -> Bool
esConvincente turista = any (dejaAcompaniado turista) . excursionesDesestresantes turista

dejaAcompaniado :: Turista -> Excursion -> Bool
dejaAcompaniado turista = not . solitario . flip hacerExcursion turista --hacemos flip xq hacerExcursion es de tipo Excursion -> Turista, entonces invierto los valores para que coincida con el tipo de dejaAcompaniado


{-3) Saber la efectividad de un tour para un conjunto de turistas. Esto se calcula como la sumatoria de la espiritualidad 
recibida de cada turista a quienes les resultó convincente el tour. La espiritualidad que recibe un turista es la suma de 
las pérdidas de stress y cansancio tras el tour. -}

efectividad :: Tour -> [Turista] -> Int
efectividad tour = sum . map (espiritualidadAportada tour) . filter (`esConvincente` tour)

espiritualidadAportada :: Tour -> Turista -> Int
espiritualidadAportada tour turista = 
    (cansancio turista - cansancio (hacerTour tour turista)) + (stress turista - stress (hacerTour tour turista))


---- PUNTO 4 ----

--Construir un tour donde se visiten infinitas playas.

playasEternas :: Tour
playasEternas = salidaLocal : repeat irALaPlaya

{-Se puede saber si ese tour es convincente para Ana? ¿Y con Beto? Justificar.

Para Ana sí porque la primer actividad ya es desestresante y siempre está acompañada.
Con Beto no se cumple ninguna de las 2 condiciones y el algoritmo diverge.

Existe algún caso donde se pueda conocer la efectividad de este tour? Justificar.

No, solamente funciona para el caso que se consulte con una lista vacía de turista, que dará siempre 0.