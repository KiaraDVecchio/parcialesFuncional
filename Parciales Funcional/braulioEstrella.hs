
import Text.Show.Functions --para poder mostrar las funciones en consola, como el type PoderBasico

-------------PERSONAJES-------------

type PoderBasico = Personaje -> Personaje
type SuperPoder = String

data Personaje = unPersonaje {
    nombre           :: String,
    poderBasico      :: PoderBasico,
    superPoder       :: SuperPoder,
    superPoderActivo :: Bool,
    cantidadDeVida   :: Int
} deriving Show

espina :: Personaje
espina = UnPersonaje {
    nombre              = "Espina",
    poderBasico         = bolaEspinosa,
    superPoder          = granadaDeEspinas 5,
    superPoderActivo    = True,
    vida                = 4800
}

pamela :: Personaje
pamela = UnPersonaje {
    nombre              = "Pamela",
    poderBasico         = lluviaDeTuercas "Sanadoras",
    superPoder          = torretaCurativa,
    superPoderActivo    = False,
    vida                = 9600
}


modificarVida :: (Int -> Int) -> Personaje -> Personaje
modificarVida unaFuncion unPersonaje = unPersonaje { cantidadDeVida = unaFuncion . cantidadDeVida $ unPersonaje }

modificarNombre :: (String -> String) -> Personaje -> Personaje
modificarNombre unaFuncion unPersonaje = unPersonaje { nombre = unaFuncion . nombre $ unPersonaje }

modificarPoderBasico :: (PoderBasico -> PoderBasico) -> Personaje -> Personaje
modificarPoderBasico unaFuncion unPersonaje = unPersonaje { poderBasico = unaFuncion . poderBasico $ unPersonaje }


-------------PODERES-------------

bolaEspinosa :: PoderBasico
bolaEspinosa unPersonaje = modificarVida (max 0 . subtract 1000) unPersonaje
--devuelvo un personaje con 1000 puntos menos de vida
--Como la vida no puede quedarme negativo, uso max 0 que me devuelve el numero mayor entre 0 y la vida: 
--si la vida luego de restarle 1000 me queda en positivo, dejo la vida como está (xq es mayor a 0) y si me
--da la vida negativa, me devuelve 0 xq es mayor a un numero negativo

granadaDeEspinas :: Int -> SuperPoder
granadaDeEspinas radio unPersonaje 
    | radio > 3 && cantidadDeVida unPersonaje < 800 = modificarSuperPoder . modificarVida (min 0) $ unPersonaje
    | radio > 3                                     = modificarNombre ("Espina Estuvo Aqui" ++) unPersonaje
    | otherwise                                     = bolaEspinosa unPersonaje
--granadaDeEspinas: el daño va a depender del radio de explosión de la misma. Si es mayor a 3, le agregara 
--a su nombre “Espina estuvo aquí”. Si además su contrincante tiene menos de 800 vida, desactiva su súper y 
--lo deja con 0 de vida. En otro caso, se usa una bola de espinas.

modificarSuperPoder :: Bool -> Personaje-> Personaje
modificarSuperPoder valor unPersonaje = unPersonaje {superPoderActivo = valor}

--lluviaDeTuercas :: String -> Personaje -> Personaje
--lluviaDeTuercas tipoDeTuerca unPersonaje
--  | tipoDeTuerca == "Sanadora" = unPersonaje { cantidadDeVida = cantidadDeVida unPersonaje + 800 }
--  | tipoDeTuerca == "Dañina"   = unPersonaje { cantidadDeVida = div (cantidadDeVida unPersonaje) 2 }
--  | otherwise                  = unPersonaje
--Toma un string que es un tipo de tuercas y toma un personaje, y devuelve el personaje con la vida cambiada

--usando pather matching (mejor) quedaría asi:
lluviaDeTuercas :: String -> PoderBasico
lluviaDeTuercas "Sanadora" unPersonaje = modificarVida (+ 800) unPersonaje
lluviaDeTuercas   "Dañina" unPersonaje = modificarVida (`div` 2) unPersonaje
lluviaDeTuercas          _ unPersonaje = unPersonaje

torretaCurativa :: SuperPoder
torretaCurativa unPersonaje = unPersonaje { superPoderActivo = True, 
        cantidadDeVida = (cantidadDeVida unPersonaje) *2}
--torretaCurativa: le activa el súper a su aliado y lo deja con el doble de su salud inicial.



atacarConElPoderEspecial :: Personaje -> Personaje
atacarConElPoderEspecial unPersonaje 
        | superPoderActivo unPersonaje = atacarConElSuperYBasico unPersonaje 
        | otherwise = unPersonaje
--atacar con el poder especial: si el personaje tiene el súper poder activo, entonces va a atacar a su 
--contrincante con el súper y con el básico. Si no, no hará nada.

atacarConElSuperYBasico :: Personaje -> Personaje
atacarConElSuperYBasico unPersonaje = (poderBasico unPersonaje . superPoder unPersonaje) unPersonaje
--Al personaje le agrego el poder basico y el superpoder 

quienesEstanEnLasUltimas :: [Personaje] -> [Personaje]
quienesEstanEnLasUltimas listaDePersonajes = filter esMenorA800 listaDePersonajes
--saber quiénes están en las últimas: el nombre de aquellos brawlers que tienen menos de 800 puntos de vida.

esMenorA800 :: Personaje -> Bool
esMenorA800 unPersonaje = cantidadDeVida unPersonaje < 800


