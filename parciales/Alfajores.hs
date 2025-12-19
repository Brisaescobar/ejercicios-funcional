-- https://docs.google.com/document/d/1m8gRD-gheA2fDbiDXc7-dpiymkhDQRbmzr-plL5BGuA/edit?tab=t.0
import Text.Show.Functions
import Data.List
import Text.Read.Lex (Number)
import GHC.Base (Float)
import Text.XHtml (rel)

-- Parte 1: ¿Que es un alfajor?
data Alfajor = Alfajor {
    relleno :: [String], 
    peso :: Int,
    dulzor :: Int,
    nombre :: String
} deriving (Show, Eq)

-- Alfajores --
jorgito :: Alfajor 
jorgito = Alfajor ["DulceDeLeche"] 80 8 "Jorgito"

havanna :: Alfajor
havanna = Alfajor ["Mousse", "Mousse"] 60 12 "Havanna"

capitanDelEspacio :: Alfajor
capitanDelEspacio = Alfajor ["DulceDeLeche"] 40 12 "Capitan del espacio"

-- b --
coeficienteDeDulzor :: Alfajor -> Int 
coeficienteDeDulzor unAlfajor = dulzor unAlfajor `div` peso unAlfajor

precioDelAlfajor :: Alfajor -> Int 
precioDelAlfajor unAlfajor = 2 * peso unAlfajor + precioSegunRelleno unAlfajor

precioPorCapa :: String -> Int 
precioPorCapa unRelleno 
    | unRelleno == "DulceDeLeche" = 12
    | unRelleno == "Mousse"       = 15
    | otherwise                   = 10

precioSegunRelleno :: Alfajor -> Int
precioSegunRelleno unAlfajor = sum (map precioPorCapa (relleno unAlfajor))

esPotable :: Alfajor -> Bool 
esPotable unAlfajor = tieneRelleno unAlfajor && lasCapasSonIguales (relleno unAlfajor)  && coeficienteDeDulzor unAlfajor >= 0
-- tiene que ser mayor a 0.1 pero no me importa

tieneRelleno:: Alfajor -> Bool
tieneRelleno = not . null . relleno

lasCapasSonIguales :: [String] -> Bool
lasCapasSonIguales rellenos = all (head rellenos ==) rellenos

-- Parte 2: Escabilidad vertical 
-- a --
abaratarUnAlfajor :: Alfajor -> Alfajor
abaratarUnAlfajor unAlfajor = unAlfajor {peso = peso unAlfajor - 10 , dulzor = dulzor unAlfajor - 7}

-- b -- 
renombarUnAlfajor ::String ->  Alfajor -> Alfajor 
renombarUnAlfajor nuevoNombre unAlfajor = unAlfajor {nombre = nuevoNombre}

-- c --
agregarUnaCapaDeRelleno :: String -> Alfajor -> Alfajor
agregarUnaCapaDeRelleno unaCapa unAlfajor = unAlfajor {relleno = unaCapa : relleno unAlfajor}

agregarPremium :: Alfajor -> Alfajor 
agregarPremium unAlfajor = unAlfajor {nombre = nombre unAlfajor ++ " " ++ "Premium"}

-- d --
alfajorPremium :: Alfajor -> Alfajor
alfajorPremium unAlfajor 
    | esPotable unAlfajor = agregarUnaCapaDeRelleno (head(relleno unAlfajor)) . agregarPremium $ unAlfajor
    | otherwise = unAlfajor

-- e -- usar Recursividad
hacerPremiumNVeces :: Int -> Alfajor -> Alfajor
hacerPremiumNVeces unaCantidad unAlfajor = hacerPremiumNVeces (unaCantidad - 1) (alfajorPremium unAlfajor)

-- f --

jorgitito :: Alfajor 
jorgitito = Alfajor ["Dulce de leche"] 70 1 "Jorgitito"

jorgelin :: Alfajor
jorgelin = Alfajor ["Dulce de leche", "Dulce de leche"] 80 8 "Jorgelin"

capitanDelEspacioCosta :: Alfajor 
capitanDelEspacioCosta = Alfajor ["Dulce de leche", "Dulce de leche", "Dulce de leche" , "Dulce de Leche"] 30 5 "Capitan del espacio de costa a costa"

-- Parte 3: Clientes del Kiosco 
type Criterio = Alfajor -> Bool
type Criterios = [Criterio]
type Alfajores = [Alfajor]

data Cliente = Cliente {
    nombreCliente :: String, 
    dinero :: Int,
    criterios:: Criterios,
    alfajores :: Alfajores
} deriving (Show)

-- Clientes --
emi :: Cliente 
emi = Cliente "Emi" 120 [criteriosSegunEmi] []

tomi :: Cliente 
tomi = Cliente "Tomi" 100 [criteriosSegunTomi] []

dante :: Cliente 
dante = Cliente "Dante" 200 [criteriosSegunDante] []

juan :: Cliente 
juan = Cliente "Juan" 500 [criteriosSegunJuan] []

-- Criterios -- 
-- EMI --
criteriosSegunEmi :: Criterio 
criteriosSegunEmi = soloMarca "Capitan del Espacio"   

soloMarca :: String -> Criterio 
soloMarca unaMarca = isInfixOf unaMarca . nombre 

-- TOMI --
criteriosSegunTomi :: Criterio 
criteriosSegunTomi unAlfajor = esPretencioso unAlfajor && esDulcero unAlfajor

esPretencioso :: Criterio
esPretencioso =  isInfixOf "premium" . nombre

esDulcero :: Criterio 
esDulcero = (>=0) . coeficienteDeDulzor

-- DANTE -- 
criteriosSegunDante :: Criterio 
criteriosSegunDante unAlfajor = noTieneDulceDeLeche unAlfajor && esExtraño unAlfajor

noTieneDulceDeLeche :: Criterio 
noTieneDulceDeLeche = notElem "Dulce de leche" . relleno

esExtraño :: Criterio 
esExtraño = not . esPotable 

-- JUAN -- 
criteriosSegunJuan :: Criterio
criteriosSegunJuan unAlfajor = esDulcero unAlfajor && esPretencioso unAlfajor && noTieneMousse unAlfajor && esJorgito unAlfajor

noTieneMousse :: Criterio 
noTieneMousse = notElem "Mousse" . relleno 

esJorgito :: Criterio 
esJorgito = isInfixOf "Jorgito" . nombre

-- b -- 
leGustanAlfajores :: Cliente -> [Alfajor] -> [Alfajor]
leGustanAlfajores unCliente = filter (leGusta unCliente)

leGusta :: Cliente -> Alfajor -> Bool 
leGusta unCliente unAlfajor = all ($ unAlfajor) (criterios unCliente)

-- c -- 
comprarAlfajor :: Alfajor -> Cliente -> Cliente 
comprarAlfajor unAlfajor unCliente 
    | dinero unCliente >= precioDelAlfajor unAlfajor = descontarDinero (precioDelAlfajor unAlfajor) . agregarAlfajor unAlfajor $ unCliente
    | otherwise = unCliente

descontarDinero :: Int -> Cliente -> Cliente 
descontarDinero unaCantidad unCliente = unCliente {dinero = dinero unCliente - unaCantidad}

agregarAlfajor :: Alfajor -> Cliente -> Cliente 
agregarAlfajor unAlfajor unCliente = unCliente {alfajores = unAlfajor : alfajores unCliente}

-- d -- 
comprarUnaLista :: [Alfajor] -> Cliente -> Cliente
comprarUnaLista alfajores unCliente = foldl (flip comprarAlfajor) unCliente (leGustanAlfajores unCliente alfajores)

