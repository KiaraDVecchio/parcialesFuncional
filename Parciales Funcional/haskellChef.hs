import Text.Show.Functions

-------PARTE A-------

data Participantes = Participante {
    nombre :: String
    truco :: [Truco]
    especialidad :: Plato
} deriving Show

type Truco = Plato -> Plato

data Plato = Plato {
  dificultad  :: Int,
  componentes :: [Componente]
} deriving Show

type Componente = (Ingrediente, Int) --Tupla
type Ingrediente = String

--TRUCOS FAMOSOS 

--endulzar: dada una cantidad de gramos de azúcar, le agrega ese componente a un plato.
endulzar :: Int -> Truco
endulzar unosGramos unPlato = agregarComponente "Azucar" unosGramos unPlato

--salar:  dada una cantidad de gramos de sal y un plato, nos retorna el mismo con esa cantidad de sal 
salar :: Int -> Truco
salar = agregarComponente "Sal"

agregarComponente :: String -> Int -> Plato -> Plato
agregarComponente unNombre unosGramos unPlato = modificarComponentes ((unNombre, unosGramos) :) unPlato

{-Explicación: ((unNombre, unosGramos) :) es una función que toma una lista de componentes y añade al 
principio de esa lista un nuevo componente (unNombre, unosGramos)}    -}

modificarComponentes :: ([Componente] -> [Componente]) -> Plato -> Plato
modificarComponentes unaFuncion unPlato = unPlato { componentes = unaFuncion . componentes $ unPlato } --Toma una función que modifica una lista de componentes y un Plato

--darSabor: dadas una cantidad de sal y una de azúcar sala y endulza un plato.
darSabor :: Int -> Int -> Truco
darSabor unosGramosDeSal unosGramosDeAzucar = endulzar unosGramosDeAzucar . salar unosGramosDeSal

--duplicarPorcion: se duplica la cantidad de cada componente de un plato… para más placer.
duplicarPorcion :: Truco
duplicarPorcion = modificarComponentes $ map duplicarCantidad --Me dice cada componente -> uso map

duplicarCantidad :: Componente -> Componente
duplicarCantidad (ingrediente, cantidad) = (ingrediente, cantidad * 2)

--simplificar: si un plato tiene más de 5 componentes y una dificultad mayor a 7 lo vamos a simplificar, sino lo 
--dejamos igual. Simplificar un plato es dejarlo con 5 de dificultad y quitarle aquellos componentes de los que hayamos agregado menos de 10 gramos.

simplificar :: Truco
simplificar unPlato
  | esComplejo unPlato = modificarComponentes (filter hayMucho) $ unPlato { dificultad = 5 }
  | otherwise          = unPlato

--unPlato { dificultad = 5 } crea una copia del plato con la dificultad cambiada a 5.
--simplificar primero verifica si el plato es complejo usando esComplejo

esComplejo :: Plato -> Bool
esComplejo unPlato = dificultad unPlato > 7 && ((> 5) . cantidadDeComponentes) unPlato

hayMucho :: Componente -> Bool
hayMucho unComponente = snd unComponente >= 10

cantidadDeComponentes :: Plato -> Int
cantidadDeComponentes unPlato = length . componentes $ unPlato


--1)esVegano: si no tiene carne, huevos o alimentos lácteos.
esVegano :: Plato -> Bool
esVegano unPlato = not . any esProductoAnimal . componentes $ unPlato

esProductoAnimal :: Componente -> Bool
esProductoAnimal (ingrediente, _) = elem ingrediente productosAnimales

productosAnimales :: [Ingrediente]
productosAnimales = ["Leche", "Carne", "Huevo", "Manteca"]

--2)esSinTacc: si no tiene harina.
esSinTacc :: Plato -> Bool
esSinTacc unPlato = not . tiene "Harina" $ unPlato --Si tiene "Harina" $ unPlato es True, not lo convierte en False.

tiene :: Ingrediente -> Plato -> Bool
tiene unIngrediente unPlato = elem unIngrediente . ingredientes $ unPlato --elem unIngrediente verifica si unIngrediente está en esa lista de ingredientes

ingredientes :: Plato -> [Ingrediente]  --Toma un Plato y devuelve una lista de Ingrediente
ingredientes unPlato = map fst . componentes $ unPlato

{-Exp: componentes unPlato obtiene la lista de componentes (cada componente es una tupla (Ingrediente, Int)).
--map fst se aplica a esta lista para extraer solo los nombres de los ingredientes (primer elemento de cada tupla). -}

--3)esComplejo: cuando tiene más de 5 componentes y una dificultad mayor a 7.
-- esComplejo -> La misma funcion que se usó para "simplificar"

--4)noAptoHipertension: si tiene más de 2 gramos de sal.
noAptoHipertension :: Plato -> Bool
noAptoHipertension unPlato = tiene "Sal" unPlato && cantidadDe "Sal" unPlato > 2

cantidadDe :: Ingrediente -> Plato -> Int  --Toma un Ingrediente y un Plato, y devuelve la cantidad de ese ingrediente en el plato.
cantidadDe unIngrediente unPlato = cantidad . conseguirComponente unIngrediente $ unPlato

cantidad :: Componente -> Int
cantidad unComponente = snd unComponente --Agarro el segundo componente de la tupla para verificar la cantidad

conseguirComponente :: Ingrediente -> Plato -> Componente --Toma un Ingrediente y un Plato, y devuelve el Componente correspondiente a ese ingrediente.
conseguirComponente unIngrediente unPlato = head . filter (esDe unIngrediente) . componentes $ unPlato -- toma el primer componente con head

esDe :: Ingrediente -> Componente -> Bool --Toma un Ingrediente y un Componente, y devuelve un Bool indicando si el componente es del ingrediente dado.
esDe unIngrediente (ingredienteDelComponente, _) = unIngrediente == ingredienteDelComponente


-------PARTE B-------

pepeRonccino :: Participante
pepeRonccino = Participante "Pepe Ronccino" [darSabor 2 5, simplificar, duplicarPorcion] unPlatoComplejo

unPlatoComplejo :: Plato --Dificulta del plato > 7 y mas de 5 componentes
unPlatoComplejo = unPlatoComplejo = Plato 10 [("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100), ("Sal", 100)]


-------PARTE C-------

--FUNCIONALIDADES

--1)cocinar: vemos como queda finalmente el plato de un participante luego de aplicar todos sus trucos a su especialidad.
cocinar :: Participante -> Plato
cocinar unParticipante = aplicarTrucos (trucos unParticipante) (especialidad unParticipante)

aplicarTrucos :: [Truco] -> Plato -> Plato
aplicarTrucos unosTrucos unPlato = foldr aplicarTruco unPlato unosTrucos --foldr toma cada truco de la lista (unosTrucos) y lo aplica al plato acumulado, comenzando desde el último truco hasta el primero.

aplicarTruco :: Truco -> Plato -> Plato
aplicarTruco unTruco unPlato = unTruco unPlato --Aplica la función unTruco al unPlato.

--2)esMejorQue: un plato es mejor que otro si tiene más dificultad pero la suma de los pesos de sus componentes es menor.
esMejorQue :: Plato -> Plato -> Bool
esMejorQue unPlato otroPlato = esMasDificil unPlato otroPlato && esMasLigero unPlato otroPlato

esMasDificil :: Plato -> Plato -> Bool
esMasDificil unPlato otroPlato = dificultad unPlato > dificultad otroPlato

esMasLigero :: Plato -> Plato -> Bool
esMasLigero unPlato otroPlato = peso unPlato < peso otroPlato

peso :: Plato -> Int
peso unPlato = sum . map cantidad . componentes $ unPlato

--3)participanteEstrella (se puede usar recursividad): Dada una lista de participantes, diremos que la estrella 
--es quien luego de que todo el grupo cocine tiene el mejor plato.

participanteEstrella :: [Participante] -> Participante
participanteEstrella unosParticipantes = foldr1 mejorParticipante unosParticipantes --En los casos donde no se puede incluir un acumulador uso foldr1

{-fold1 aplica la función mejorParticipante recursivamente a todos los elementos de la lista, comparando dos 
participantes a la vez, hasta encontrar el mejor participante. -}

mejorParticipante :: Participante -> Participante -> Participante
mejorParticipante unParticipante otroParticipante
  | esMejorQue (cocinar unParticipante) (cocinar otroParticipante) = unParticipante
  | otherwise = otroParticipante

{-Con cocinar se aplican todos los trucos a un plato y compara los platos cocinados por dos participantes 
usando la función esMejorQue -}

{- Usando recursividad: 
participanteEstrella' :: [Participante] -> Participante
participanteEstrella' [unParticipante] = unParticipante
participanteEstrella' (unParticipante : otrosParticipantes) =
  mejorParticipante unParticipante (participanteEstrella' otrosParticipantes)

Caso base: Si la lista contiene solo un participante, devuelve ese participante.
Caso recursivo: Compara el primer participante con el mejor participante del resto de la lista (encontrado recursivamente). -}


-------PARTE D-------

--platinum: Este plato tiene infinitos componentes misteriosos con cantidades incrementales y dificultad 10
platinum :: Plato
platinum = Plato 10 unaListaDecomponentesRara

unaListaDecomponentesRara :: [Componente]
unaListaDecomponentesRara =
  map (\unNumero -> ("Ingrediente " ++ show unNumero, unNumero)) [1..]

{-La función lamda toma un número unNumero y lo transforma en un componente de la forma 
("Ingrediente X", X), donde X es el número. 
Para 1, produce ("Ingrediente 1", 1).
Para 2, produce ("Ingrediente 2", 2).
Para 3, produce ("Ingrediente 3", 3).
Y así sucesivamente.   -}