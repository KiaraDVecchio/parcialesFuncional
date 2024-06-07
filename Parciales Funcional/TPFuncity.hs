module Ejemplo where
import Text.Show.Functions()


data Ciudad = Ciudad {
    nombre                 :: String,
    anioFundacion          :: Int,
    atraccionesPrincipales :: [Atraccion],
    costoDeVida            :: Int
}   deriving Show

type Atraccion = String


baradero :: Ciudad
baradero = Ciudad "Baradero" 1615 ["Parque del Este", "Museo Alejandro Barbich"] 150

nullish :: Ciudad
nullish = Ciudad "Nullish" 1800 [] 140

caletaOlivia :: Ciudad
caletaOlivia = Ciudad "Caleta Olivia" 1901 ["El Gorosito", "Faro Costanera"] 120

maipu :: Ciudad
maipu = Ciudad "Maipu" 1878 ["Fortin Kakel"] 115

azul :: Ciudad
azul = Ciudad "Azul" 1832 ["Teatro español", "Parque municipal Sarmiento", "Costanera Cacique Catriel"] 190


--Punto 1: Valor de una ciudad

valorCiudad :: Ciudad -> Int
valorCiudad ciudad
    | anioFundacion ciudad < 1800 = 5 * (1800 - anioFundacion ciudad)
    | null (atraccionesPrincipales ciudad) = costoDeVida (modificarCostoDeVida (*2) ciudad)
    | otherwise = costoDeVida (modificarCostoDeVida (*3) ciudad)

modificarCostoDeVida :: (Int -> Int) -> Ciudad -> Ciudad
modificarCostoDeVida unaFuncion ciudad = ciudad {costoDeVida = unaFuncion . costoDeVida $ ciudad}


--Punto 2: Alguna atraccion copada - Ciudad sobria - Ciudad con nombre raro

isVowel :: Char -> Bool
isVowel character = character `elem` "aeiouAEIOU"

tieneAtraccionCopada :: Ciudad -> Bool
tieneAtraccionCopada  = any (isVowel . head) . atraccionesPrincipales

esCiudadSobria :: Int -> Ciudad ->  Bool
esCiudadSobria letrasMinimas ciudad = all ((>letrasMinimas) . length) (atraccionesPrincipales ciudad)

tieneNombreRaro :: Ciudad -> Bool
tieneNombreRaro ciudad = (<5) . length . nombre $ ciudad


--Punto 3:

aplicarPorcentaje :: Float -> Evento
aplicarPorcentaje porcentaje ciudad = modificarCostoDeVida (\costoDeVida -> round (fromIntegral costoDeVida * porcentaje)) ciudad
-- round redondea un float al entero mas cercano que es el tipo de dato esperado por la funcion
-- fromIntegral convierte al entero en float para poder realizar la multiplicacion 

type Evento = Ciudad -> Ciudad

sumarNuevaAtraccion :: String -> Evento
sumarNuevaAtraccion nuevaAtraccion ciudad = modificarAtraccion (nuevaAtraccion :) . aplicarPorcentaje 1.2 $ ciudad

crisis :: Evento
crisis ciudad = modificarAtraccion (drop 1) . aplicarPorcentaje 0.9 $ ciudad

modificarAtraccion :: ([Atraccion] -> [Atraccion]) -> Ciudad -> Ciudad
modificarAtraccion funcion ciudad = ciudad {atraccionesPrincipales = funcion . atraccionesPrincipales $ ciudad}

remodelacion :: Int -> Ciudad -> Ciudad
remodelacion porcentaje ciudad = modificarCostoDeVida (\costoDeVida -> costoDeVida + costoDeVida * porcentaje `div` 100) $ ciudad { nombre = "New " ++ nombre ciudad }

reevaluacion :: Int -> Evento
reevaluacion letras ciudad 
    | esCiudadSobria letras ciudad = aplicarPorcentaje 1.1 ciudad
    | otherwise = modificarCostoDeVida (subtract 3 ) ciudad

--Punto 4:
{-
    reevaluacion 13 . crisis . remodelacion 50 . sumarNuevaAtraccion "Balneario Municipal Alte. Guillermo Brown" $ azul

    Ciudad {nombre = "New Azul", anioFundacion = 1832, atraccionesPrincipales = ["Teatro espa\241ol","Parque municipal Sarmiento","Costanera Cacique Catriel"], costoDeVida = 339}
-}

----------SEGUNDA PARTE----------

--Punto 1.1


data Año = UnAño {
    numeroDeAño :: Int,
    eventos :: [Evento]
} deriving Show

año2022 :: Año 
año2022 = UnAño 2022 [crisis, remodelacion 5, reevaluacion 7]

año2015 :: Año
año2015 = UnAño 2015 []

aplicarEventos :: [Evento] -> Ciudad -> Ciudad
aplicarEventos eventos ciudad = foldl (\ciudad evento -> evento ciudad) ciudad eventos

pasoDeAño :: Año -> Ciudad -> Ciudad
pasoDeAño año ciudad = aplicarEventos (eventos año) ciudad

--Punto 1.2

type Criterio = Ciudad -> Int

subioRespectoACriterio :: Criterio -> Ciudad -> Evento -> Bool
subioRespectoACriterio criterio ciudad evento = compararCiudades (>) (evento ciudad) ciudad criterio

numeroDeAtracciones :: Criterio
numeroDeAtracciones = length . atraccionesPrincipales

compararCiudades :: (Int -> Int -> Bool) -> Ciudad -> Ciudad -> Criterio -> Bool
compararCiudades operador ciudad1 ciudad2 criterio = operador (criterio ciudad1) (criterio ciudad2)

--Punto 1.3

eventosQue :: Año -> Ciudad -> (Ciudad -> Evento -> Bool) -> [Evento]
eventosQue año ciudad condicion = filter (condicion ciudad) (eventos año)

subeCostoDeVida :: Ciudad -> Evento -> Bool
subeCostoDeVida = subioRespectoACriterio costoDeVida 

aplicarSiCumple :: (Ciudad -> Evento -> Bool) -> Año -> Ciudad -> Ciudad
aplicarSiCumple condicion año ciudad = aplicarEventos (eventosQue año ciudad condicion) ciudad

aplicarSiSubeCostoDeVida :: Año -> Ciudad -> Ciudad
aplicarSiSubeCostoDeVida = aplicarSiCumple subeCostoDeVida

--Punto 1.4

bajoRespectoACriterio :: Criterio -> Ciudad -> Evento -> Bool
bajoRespectoACriterio criterio ciudad evento = compararCiudades (<) (evento ciudad) ciudad criterio

bajaCostoDeVida :: Ciudad -> Evento -> Bool
bajaCostoDeVida = bajoRespectoACriterio costoDeVida 
 
aplicarSiBajoCostoDeVida :: Año -> Ciudad -> Ciudad
aplicarSiBajoCostoDeVida = aplicarSiCumple bajaCostoDeVida

--Punto 1.5


subeValor :: Ciudad -> Evento -> Bool
subeValor = subioRespectoACriterio valorCiudad 

aplicarSiValorSube :: Año -> Ciudad -> Ciudad
aplicarSiValorSube = aplicarSiCumple subeValor

--Punto 2

año2023 :: Año
año2023 = UnAño 2023 [crisis, sumarNuevaAtraccion "parque", remodelacion 10, remodelacion 20]

año2021 :: Año
año2021 = UnAño 2021 [crisis, sumarNuevaAtraccion "playa"]

--Punto 2.1
estaOrdenadaSegun :: (a -> Int) -> [a] -> Bool
estaOrdenadaSegun _ [] = False
estaOrdenadaSegun _ [_] = True
estaOrdenadaSegun funcion (elemento1 : elemento2 : elementosRestantes) = funcion elemento1 <= funcion elemento2 && estaOrdenadaSegun funcion (elemento2 : elementosRestantes)

costoDeVidaAplicandoEvento :: Evento -> Ciudad -> Int
costoDeVidaAplicandoEvento evento ciudad = (costoDeVida . evento) ciudad

eventosOrdenados :: Año -> Ciudad -> Bool
eventosOrdenados (UnAño _ eventos) ciudad =  estaOrdenadaSegun ( flip costoDeVidaAplicandoEvento ciudad) eventos

--Punto 2.2

ciudadesOrdenadas :: Evento -> [Ciudad] -> Bool
ciudadesOrdenadas evento ciudades =  estaOrdenadaSegun (costoDeVidaAplicandoEvento evento) ciudades

--Punto 2.3

añosOrdenados :: [Año] -> Ciudad -> Bool
añosOrdenados años ciudad = estaOrdenadaSegun (\año -> costoDeVida (pasoDeAño año ciudad)) años

--Punto 3

infinitasRemodelaciones :: [Evento]
infinitasRemodelaciones = map remodelacion [1..]

año2024 :: Año
año2024 = UnAño 2024 (crisis : reevaluacion 7 : infinitasRemodelaciones)

-- Pregunta: Si, puede haber un resultado posible, ya que al usar lazy evaluation se ira comprobando evento a evento si estan ordenados.Al aplicar remodelacion 1 ya la lista 
-- sera no ordenada y por ende dara falso.

discoRayado :: [Ciudad]
discoRayado = (azul : nullish : intercalarInfinito)

intercalarInfinito :: [Ciudad]
intercalarInfinito = cycle [caletaOlivia, baradero ]

-- Pregunta: Si, puede haber un resultado posible, ya que al haskell hacer uso del lazy evaluation directamente desde un principio, al evaluar azul con nullish nos dara falso por lo 
-- que las infinitas ciudades no tendran relevancia en el resultado

laHistoriaSinFin :: [Año]
laHistoriaSinFin = (año2021 : año2022 : infinitosAños2023)

infinitosAños2023 :: [Año]
infinitosAños2023 = repeat año2023

-- Pregunta: Si, puede haber un resultado posible, ya que al usar lazy evaluation se ira comprobando año a año si estan ordenados.Al aplicar año 2021 y luego el año 2022 ya la lista 
-- sera no ordenada y por ende dara falso.En cambio, si invertimos el año de estos dos, el programa quedara tildado ya que es verdadero y se quedara evaluando la lista infinitamente
