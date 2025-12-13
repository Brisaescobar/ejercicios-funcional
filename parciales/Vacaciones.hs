{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Hoist not" #-}
import Text.Show.Functions
import Data.List

data Turista = Turista {
    cansancio :: Int, 
    estres :: Int, 
    solitario :: Bool, 
    idiomas :: [String]
} deriving (Show, Eq)

-- funcion que dan en el parcial f es (a -> Int)
deltaSegun :: (a -> Int) -> a -> a -> Int
deltaSegun f algo1 algo2 = f algo1 - f algo2

-- Turistas punto 1
ana :: Turista 
ana = Turista { cansancio = 0, estres = 21, solitario = False, idiomas = ["Español"]}

beto :: Turista 
beto = Turista { cansancio = 15, estres = 15, solitario = True, idiomas = ["Aleman"]}

cathy :: Turista
cathy = Turista { cansancio = 15, estres = 15, solitario = True, idiomas = ["Aleman", "Catalan"]}

-- funciones que repiten logica
cambiarEstres :: Int -> Turista -> Turista
cambiarEstres num unturista = unturista {estres = estres unturista + num}

cambiarCansancio :: Int -> Turista -> Turista 
cambiarCansancio num unturista = unturista {cansancio = cansancio unturista + num}

nivelDeIntensidad :: Int -> Int 
nivelDeIntensidad minutos = div minutos 4 
--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

type Excursion = Turista -> Turista

-- Excursiones
irALaPlaya :: Excursion
irALaPlaya unturista 
    | solitario unturista = cambiarCansancio (-5) unturista
    | otherwise = cambiarEstres (-1) unturista

apreciarAlgunElementoDelPaisaje:: String -> Excursion 
apreciarAlgunElementoDelPaisaje unpaisaje = cambiarEstres (-length unpaisaje)  --Saco un turista 

salirAHablarUnIdiomaEspecifico :: String -> Excursion 
salirAHablarUnIdiomaEspecifico unidioma unturista = unturista {idiomas = idiomas unturista ++ [unidioma], solitario = False}  

caminarCiertosMinutos :: Int -> Excursion 
caminarCiertosMinutos minutos = cambiarEstres (-(nivelDeIntensidad minutos)) . cambiarEstres (-(nivelDeIntensidad minutos))

data Marea = Fuerte | Moderada | Tranquila deriving (Show, Eq)
paseoEnBarco :: Marea -> Excursion
paseoEnBarco Fuerte = cambiarCansancio 10 . cambiarEstres 6
paseoEnBarco Moderada = id -- devuelve lo mismo
paseoEnBarco Tranquila = caminarCiertosMinutos 10 . apreciarAlgunElementoDelPaisaje "Mar" . salirAHablarUnIdiomaEspecifico "Aleman"
--- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- --- 

-- 2a
hacerExcursion :: Excursion -> Turista -> Turista 
hacerExcursion unaexcursion unturista = reducirEstres 10 (unaexcursion unturista)

reducirEstres :: Int -> Turista -> Turista
reducirEstres porcentaje unturista = cambiarEstres (- div (porcentaje * estres unturista) 100 ) unturista 

-- 2b
deltaExcursionSegun :: (Turista -> Int) -> Turista -> Excursion -> Int
deltaExcursionSegun f unturista unaexcursion = deltaSegun f (hacerExcursion unaexcursion unturista) unturista

-- 2c
esEducativa :: Turista -> Excursion -> Bool 
esEducativa unturista unaexcursion = deltaExcursionSegun (length . idiomas) unturista unaexcursion >0  

excursionesDesestresantes :: Turista -> Tours -> [Excursion]
excursionesDesestresantes unturista  = filter (esDesestresante unturista)  -- saco un tour

esDesestresante :: Turista -> Excursion -> Bool
esDesestresante turista excursion = deltaExcursionSegun estres turista excursion <= (-3)

-- 3 
type Tours = [Excursion] --deberia ser mas cortos los nombres

completo :: Tours 
completo = [caminarCiertosMinutos 20, apreciarAlgunElementoDelPaisaje "Cascada", caminarCiertosMinutos 40, irALaPlaya, salirAHablarUnIdiomaEspecifico "Melmacquiano"]

ladoB :: Excursion -> Tours 
ladoB unaexcursion = [paseoEnBarco Tranquila, unaexcursion, caminarCiertosMinutos 120]

islaVecina :: Marea -> Tours 
islaVecina mareadelaisla = [paseoEnBarco mareadelaisla, excursionIslaVecina mareadelaisla, paseoEnBarco mareadelaisla]

-- recibe la marea y aprecia el paisaje
excursionIslaVecina :: Marea -> Excursion 
excursionIslaVecina Fuerte = apreciarAlgunElementoDelPaisaje "Lago"
excursionIslaVecina _ = irALaPlaya

-- 4
hacerTour :: Turista -> Tours -> Turista
hacerTour unturista untour = foldl (flip hacerExcursion) (cambiarEstres (length untour) unturista) untour

tourConvincente :: Turista -> [Tours] -> Bool
tourConvincente unturista = any (esConvincente unturista)

esConvincente :: Turista -> Tours -> Bool
esConvincente unturista untour = any (not . solitario . ($ unturista)) (excursionesDesestresantes unturista untour)

efectividadDelTour :: Tours -> [Turista] -> Int
efectividadDelTour untour = sum . map (espiritualidadAportada untour) . filter ( `esConvincente` untour)

espiritualidadAportada :: Tours -> Turista -> Int
espiritualidadAportada untour = negate . deltaRutina untour

deltaRutina :: Tours -> Turista -> Int
deltaRutina untour unturista = deltaSegun nivelDeRutina (hacerTour unturista untour) unturista

nivelDeRutina :: Turista -> Int
nivelDeRutina unturista = cansancio unturista + estres unturista

playasinfinitas :: Tours 
playasinfinitas = repeat irALaPlaya

-- repeat :: a -> [a] 