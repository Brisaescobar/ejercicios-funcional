import Text.Show.Functions
import Data.List

-- punto 1: una aventura es mas divertida si huele a peligro ...
type Criterio = Aventurero -> Bool

data Aventurero = Aventurero {
    nombre :: String,
    carga :: Int, 
    salud :: Int, -- 0 a 100
    coraje :: Bool,
    criterio :: Criterio
} deriving (Show)

-- Criterios -- 
conformista :: Criterio 
conformista _ = True 

valiente :: Criterio 
valiente unAventurero = coraje unAventurero || salud unAventurero >= 50 

lightPacker :: Int -> Criterio 
lightPacker umbral unAventurero = carga unAventurero < umbral 

-- Punto 2: Casi raiders of the lost ark

-- a -- 
nombreMasDeCinco :: [Aventurero] -> Bool
nombreMasDeCinco = any ((> 5) . length . nombre)

-- b -- 
sumarCargaTotal :: [Aventurero] -> Int 
sumarCargaTotal = sum . map carga . filter (even . carga)

-- Punto 3: Ke personajes 
type Encuentro = Aventurero -> Aventurero
souvenir :: Encuentro 
souvenir = modificarCarga (-1) 

ispirador :: Encuentro
ispirador = modificarCoraje True . modificarSalud (+ 10) . souvenir

embaucador :: Encuentro
embaucador = modificarCoraje False . dividirCargaALaMitad . modificarCriterio (lightPacker 10) .souvenir

-- Funciones -- 
modificarCarga :: Int -> Aventurero -> Aventurero 
modificarCarga unaCantidad unAventurero = unAventurero {carga = carga unAventurero + unaCantidad}

dividirCargaALaMitad :: Aventurero -> Aventurero 
dividirCargaALaMitad unAventurero = unAventurero {carga = carga unAventurero `div` 2 }

modificarSalud :: (Int -> Int) -> Aventurero -> Aventurero
modificarSalud func unAventurero = unAventurero {salud = min 100 (func (salud unAventurero))}

modificarCoraje :: Bool -> Aventurero -> Aventurero 
modificarCoraje nuevoCoraje unAventurero = unAventurero {coraje = nuevoCoraje}

modificarCriterio :: Criterio -> Aventurero -> Aventurero
modificarCriterio nuevoCriterio unAventurero = unAventurero {criterio = nuevoCriterio}

-- punto 4: ¿a que encuentros se enfrentaria un aventurero? usar recursividad
type Encuentros = [Encuentro]

-- aQueEncuentrosSeEnfrenta :: [Encuentro] -> Aventurero -> [Encuentro]
-- aQueEncuentrosSeEnfrenta [] _ = []
-- aQueEncuentrosSeEnfrenta (encuentro:restoEncuentros) aventurero 
   -- | cumpleCriterio = encuentro : aQueEncuentrosSeEnfrenta restoEncuentros aventureroNuevo
   -- | otherwise      = []
   -- where 
     --   aventureroNuevo = encuentroConPersonaje encuentro aventurero
     --  cumpleCriterio  = (criterioEncuentros aventurero) aventureroNuevo