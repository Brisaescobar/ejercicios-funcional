import Text.Show.Functions
import Data.List
import GHC.Base (BCO)

data Persona = Persona { 
    estres :: Int, 
    nombre :: String,
    preferencias :: [Preferencia],
    amigos :: Int
} deriving (Show, Eq)

type Contingente = [Persona]

type Preferencia = String

-- Punto 1 -- 
estresDeGenteGlotona :: Contingente -> Int 
estresDeGenteGlotona  = sum . map estres . filter (elem "Gastronomia" . preferencias)

esContingenteRaro :: Contingente -> Bool 
esContingenteRaro = all (even . amigos)

-- Punto 2 --
type PlanTuristico = Persona -> Persona 

villaGesell :: String -> PlanTuristico
villaGesell unMes unaPersona
    | unMes == "Enero" || unMes == "Febrero"  = aumentarEstres 10 unaPersona
    | otherwise = disminuirEstresAlaMitad unaPersona

-- podria poner 1 o 2 antes que enero o febrero

aumentarEstres :: Int -> Persona -> Persona
aumentarEstres unaCantidad unaPersona = unaPersona {estres = estres unaPersona + unaCantidad}

disminuirEstresAlaMitad :: Persona -> Persona 
disminuirEstresAlaMitad unaPersona = unaPersona {estres = estres unaPersona `div` 2}

lasToninas :: Bool -> PlanTuristico 
lasToninas conPlata unaPersona 
    | conPlata = disminuirEstresAlaMitad unaPersona 
    | otherwise = aumentarEstres (10 * amigos unaPersona) unaPersona

puertoMadryn :: PlanTuristico 
puertoMadryn unaPersona = unaPersona {amigos = amigos unaPersona + 1}

--laAdela :: PlanTuristico
--laAdela = 

-- a -- 
esPiola :: Persona -> [PlanTuristico] -> Bool 
esPiola unaPersona = any ((< estres unaPersona). estres . ($ unaPersona))

-- b -- 
ana :: Persona 
ana = Persona 100  "Ana" ["Mar", "Gastronomia"] 3 

planTuristicoAna :: [PlanTuristico]
planTuristicoAna = [villaGesell "Enero" , lasToninas True, puertoMadryn]

resultadoAna :: Bool
resultadoAna = esPiola ana planTuristicoAna

-- c -- 
pedro :: Persona
pedro = Persona 20 "Luis" ["Montaña"] 5 

planTuristicoPedro :: [PlanTuristico]
planTuristicoPedro = [villaGesell "Febrero" , lasToninas False, puertoMadryn]

resultadoPedro :: Bool
resultadoPedro = esPiola pedro planTuristicoPedro
