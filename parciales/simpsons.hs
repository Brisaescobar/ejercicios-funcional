import Text.Show.Functions
import Data.List
import Text.Read.Lex (Number)
import GHC.Base (Float)

data Personaje = Personaje {
    nombre :: String, 
    dinero :: Int, 
    felicidad :: Int
} deriving (Show, Eq)

-- types necesarios --

type Trabajo = String 
type Actividad = Personaje -> Personaje
type Actividades = [Actividad]
type Logro = Personaje -> Bool

-- Personajes --

burns :: Personaje 
burns = Personaje "Sr. Burns" 100000000 0

lisa :: Personaje 
lisa = Personaje "Lisa" 2000 10

-- aumentar y disminur felicidad y modificar dinero --

aumentarFelicidad :: Int -> Personaje -> Personaje
aumentarFelicidad unaCantidad unPersonaje = unPersonaje {felicidad = max 0 (felicidad unPersonaje + unaCantidad)}

disminuirFelicidad :: Int -> Personaje -> Personaje 
disminuirFelicidad unaCantidad unPersonaje = unPersonaje {felicidad = felicidad unPersonaje - unaCantidad}

modificarDinero :: Int -> Personaje -> Personaje 
modificarDinero unaCantidad unPersonaje = unPersonaje  {dinero = dinero unPersonaje + unaCantidad}

-- actividades -- 

irALaEscuela :: Actividad
irALaEscuela unPersonaje 
    |   nombre unPersonaje == "Lisa" = aumentarFelicidad 20 unPersonaje
    |   otherwise = disminuirFelicidad 20 unPersonaje

comerDonas :: Int -> Actividad 
comerDonas unaCantidad =  aumentarFelicidad (unaCantidad * 10) . disminuirFelicidad (10 * unaCantidad)

irATrabajar :: String -> Actividad 
irATrabajar unTrabajo = modificarDinero (length unTrabajo)

trabajarComoDirector :: Actividad 
trabajarComoDirector = irATrabajar "Escuela elemental" . disminuirFelicidad 20

-- Logros --
serMillonario :: Logro
serMillonario unPersonaje = dinero unPersonaje > dinero burns 

alegrarse :: Int -> Logro 
alegrarse felicidadDeseada unPersonaje = felicidad unPersonaje > felicidadDeseada

verElProgramaDeKrosty :: Logro
verElProgramaDeKrosty unPersonaje = (>= 10) (dinero unPersonaje)

-- A -- 
esActividadDecisiva :: Personaje -> Logro -> Actividad -> Bool
esActividadDecisiva unPersonaje unLogro unaActividad = unLogro (unaActividad unPersonaje)

-- B -- 
actividadesDecisivas :: Personaje -> Logro -> Actividades -> Actividades 
actividadesDecisivas unPersonaje unLogro = filter (esActividadDecisiva unPersonaje unLogro)

-- C -- 
listaInfinitaDeActividades :: Actividad -> Actividades
listaInfinitaDeActividades = repeat 
