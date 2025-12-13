import Text.Show.Functions
import Data.List

data Auto = Auto {
    color :: String,
    velocidad :: Int,
    distancia :: Int
} deriving (Show, Eq)

type Carrera = [Auto]

-- punto 1
estaCerca :: Auto -> Auto -> Bool 
estaCerca auto1 auto2 = auto1 /= auto2 && distanciaEntreAutos auto1 auto2 <10

--funcion para calcular la distancia entre autos el abs de la distancia del auto1 - distancia de un auto
distanciaEntreAutos :: Auto -> Auto -> Int
distanciaEntreAutos auto1 = abs . (distancia auto1 -) . distancia 

vaTranquilo :: Auto -> Carrera ->Bool 
vaTranquilo unauto unacarrera = (not . tieneAlgunAutoCerca unauto) unacarrera && vaGanando unauto unacarrera 

--busco en toda la lista de auto si tiene cerca otro 
tieneAlgunAutoCerca :: Auto -> Carrera -> Bool 
tieneAlgunAutoCerca unauto = any (estaCerca unauto)

vaGanando :: Auto -> Carrera -> Bool 
vaGanando unauto = all (leVaGanando unauto) . filter (/= unauto)

leVaGanando :: Auto -> Auto -> Bool
leVaGanando ganador = ( < distancia ganador) . distancia 

-- recible un auto y la carrera me devuelve el puesto
-- invierte el resultado de le va ganando filta los autos y cuenta cuantos autos hay que le ganan a unauto 
-- y le suma uno por si no hay ninguno
puesto :: Auto -> Carrera -> Int 
puesto unauto = (1+) . length . filter (not . leVaGanando unauto)

-- punto 2 
correr :: Int -> Auto -> Auto 
correr tiempo unauto = unauto {distancia = distancia unauto + tiempo * velocidad unauto}

--recibe un un num un auto y modifica la velocidad del auto
alterarLaVelocidad :: (Int -> Int) -> Auto -> Auto
alterarLaVelocidad modificarvelocidad unauto = unauto {velocidad = modificarvelocidad (velocidad unauto)}

-- recibe la cantidad un auto /v es una funcion anonima resta la cantidad a la velocidad 
-- y se queda con el max  entre resultado 0 para no devolver negativos

bajarVelocidad :: Int -> Auto -> Auto
bajarVelocidad cantidad = alterarLaVelocidad (\v -> max 0 (v - cantidad))

type Powerups = Auto -> Carrera -> Carrera 

-- funcion que dan en el parcial
afectarALosQueCumplen :: (a -> Bool) -> (a -> a) -> [a] -> [a]
afectarALosQueCumplen criterio efecto lista = (map efecto . filter criterio) lista ++ filter (not . criterio) lista

terremoto :: Powerups 
terremoto autoquegatillo = afectarALosQueCumplen (estaCerca autoquegatillo) (bajarVelocidad 50)

miguelitos :: Int -> Powerups 
miguelitos cantidad autoquegatillo = afectarALosQueCumplen (leVaGanando autoquegatillo) (bajarVelocidad cantidad)

-- esta mal la logica no se que mierda hacer 
-- jetpack :: Powerups 
-- jetpack autoquegatillo tiempo = afectarAlosQueCumplen (alterarvelocidd /v -> velocidad autoquegatillo) . correr tiempo . alterarVelocidad (*2)

type Evento = Carrera -> Carrera 
type Color = String 

-- simularCarrera :: Carrera -> [Carrera -> Carrera] -> [(Int , Color)]
-- simularCarrera unacarrera evento = (  evento) unacarrera

correnTodos :: Int -> Evento
correnTodos tiempo = map (correr tiempo) 

usaPowerUp :: (Auto -> Carrera -> Evento) -> Color -> Carrera -> Evento
usaPowerUp powerups colorBuscado carrera = powerups (head (filter (\auto -> color auto == colorBuscado) carrera)) carrera


