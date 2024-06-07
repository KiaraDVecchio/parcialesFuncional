
{-1-} 
aplicarEventos :: [Evento] -> Ciudad -> Ciudad
aplicarEventos eventos ciudad = foldl (\ciudad evento -> evento ciudad) ciudad eventos

fold soluciona el problema de repetición lógica. Aplica una función de manera acumulativa a los 
elementos de una lista, empezando por el primer elemento y recorriendo la lista hacia la derecha 
(de izquierda a derecha).

Esta función toma una lista de eventos y una ciudad inicial, y devuelve la ciudad resultante después 
de aplicar todos los eventos.

foldl (\ciudad evento -> evento ciudad)
Esta función toma una ciudad (el acumulador) y un evento (un elemento de la lista de eventos), y aplica 
el evento a la ciudad, devolviendo una nueva ciudad.

{-2-}
subioRespectoACriterio :: Ciudad -> Criterio -> Evento -> Bool
subioRespectoACriterio ciudad criterio evento = compararCiudades (evento ciudad) ciudad criterio

Funcionamiento: Aplica el evento a la ciudad para obtener una nueva ciudad y luego compara la ciudad 
resultante con la ciudad original usando el criterio dado, delegando esta comparación a la función 
compararCiudades.

{-3-}
eventosQueSubanCostoDeVida :: Año -> Ciudad -> [Evento]
eventosQueSubanCostoDeVida año ciudad = filter (subioCostoDeVida ciudad) (eventos año)

Funcionamiento: Obtiene los eventos del año (eventos año), filtra esos eventos usando la función 
subioCostoDeVida, que verifica si cada evento aumenta el costo de vida de la ciudad y evuelve una lista 
de eventos que incrementan el costo de vida.