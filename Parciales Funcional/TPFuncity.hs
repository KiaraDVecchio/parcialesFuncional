module Ejemplo where
import Text.Show.Functions()

data Ciudad = Ciudad {
    nombre                 :: String,
    anioFundacion          :: Int,
    atraccionesPrincipales :: [Atracciones],
    costoDeVida            :: Int
}   deriving Show

type Atracciones = String


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
sumarNuevaAtraccion nuevaAtraccion ciudad = aplicarPorcentaje 1.2 ciudad {atraccionesPrincipales = nuevaAtraccion : atraccionesPrincipales ciudad}

crisis :: Evento
crisis ciudad = aplicarPorcentaje 0.9 ciudad {atraccionesPrincipales = drop 1 (atraccionesPrincipales ciudad)}

remodelacion :: Int -> Ciudad -> Ciudad
remodelacion porcentaje ciudad = modificarCostoDeVida (\costoDeVida -> costoDeVida + costoDeVida * porcentaje `div` 100) $ ciudad { nombre = "New " ++ nombre ciudad }

reevaluacion :: Int -> Evento
reevaluacion letras ciudad 
    | esCiudadSobria letras ciudad = aplicarPorcentaje 1.1 ciudad
    | otherwise = modificarCostoDeVida (\costoDeVida -> costoDeVida - 3) ciudad

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

{-1-} 
aplicarEventos :: [Evento] -> Ciudad -> Ciudad
aplicarEventos eventos ciudad = foldl (\ciudad evento -> evento ciudad) ciudad eventos

pasoDeAño :: Año -> Ciudad -> Ciudad
pasoDeAño año ciudad = aplicarEventos (eventos año) ciudad

--Punto 1.2

type Criterio = Ciudad -> Int
{-2-}
subioRespectoACriterio :: Ciudad -> Criterio -> Evento -> Bool
subioRespectoACriterio ciudad criterio evento = compararCiudades (evento ciudad) ciudad criterio

compararCiudades :: Ciudad -> Ciudad -> Criterio -> Bool
compararCiudades ciudad1 ciudad2 criterio = criterio ciudad1 > criterio ciudad2

numeroDeAtracciones :: Criterio
numeroDeAtracciones = length . atraccionesPrincipales

--Punto 1.3
{-3-}
eventosQueSubanCostoDeVida :: Año -> Ciudad -> [Evento]
eventosQueSubanCostoDeVida año ciudad = filter (subioCostoDeVida ciudad) (eventos año)

subioCostoDeVida :: Ciudad -> Evento -> Bool
subioCostoDeVida ciudad evento = subioRespectoACriterio ciudad costoDeVida evento

aplicarSiSubeCostoDeVida :: Año -> Ciudad -> Ciudad
aplicarSiSubeCostoDeVida año ciudad = aplicarEventos (eventosQueSubanCostoDeVida año ciudad) ciudad