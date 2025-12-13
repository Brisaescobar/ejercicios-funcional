import Text.Show.Functions
import Data.List

data Guerrero = Guerrero {
    nombre :: String, 
    ki :: Float,
    cansancio :: Float
} deriving (Show, Eq)

-- participantes 
goku :: Guerrero 
goku = Guerrero "goku" 1000 3000 

modificarKi :: Float -> Guerrero -> Guerrero 
modificarKi  unnum unguerrero = unguerrero { ki = ki unguerrero * unnum}

modificarCansancio :: Float -> Guerrero -> Guerrero
modificarCansancio unnum unguerrero = unguerrero { cansancio = cansancio unguerrero * unnum}

type Ejercicio =  Guerrero -> Guerrero 

flexionesDeBrazo :: Ejercicio
flexionesDeBrazo  = modificarCansancio 1.5 -- saque un guerrero

saltarCajon :: Float -> Ejercicio 
saltarCajon altura = modificarKi (altura / 10.0) . modificarCansancio (altura / 5.0) -- saque un guerrero

snatch :: Ejercicio
snatch unguerrero 
  | ki unguerrero >= 22000 = (modificarCansancio 1.10 . modificarKi 1.05) unguerrero
  | otherwise              = modificarCansancio 100 unguerrero

estacansado :: Guerrero -> Bool 
estacansado unguerrero = cansancio unguerrero > 0.44 * ki unguerrero

estaExahusto :: Guerrero -> Bool 
estaExahusto unguerrero = cansancio unguerrero > 0.72 * ki unguerrero 

realizarEjercico :: Guerrero -> Ejercicio -> Guerrero 
realizarEjercico unguerrero unejercicio = unejercicio unguerrero

descansar :: Int ->  Guerrero -> Guerrero
descansar minutos unguerrero =  unguerrero { cansancio = cansancio unguerrero - fromIntegral (sumatoriaDeDescanso minutos)}

sumatoriaDeDescanso :: Int -> Int 
sumatoriaDeDescanso n = sum [1..n]



