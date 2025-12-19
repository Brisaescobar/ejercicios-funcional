import Text.Show.Functions
import Data.List
import Graphics.Win32 (mB_APPLMODAL)
-- 1 -- 
data Mago = Mago {
    nombre :: String, 
    edad :: Int, 
    salud :: Int, 
    hechizos :: [Hechizo]
}

type Hechizo = Mago -> Mago 

-- Hechizos --
curar :: Int -> Hechizo 
curar unaCantidad unMago = modificarSalud unaCantidad unMago

lanzarRayo :: Hechizo 
lanzarRayo unMago 
    | salud unMago > 10 = modificarSalud (-10) unMago 
    | otherwise         = unMago {salud = salud unMago `div` 2}

amnesia :: Int -> Hechizo 
amnesia n unMago = unMago {hechizos = drop n (hechizos unMago)}

confundir :: Hechizo 
confundir unMago = (head (hechizos unMago)) unMago

modificarSalud :: Int -> Mago -> Mago 
modificarSalud unaCantidad unMago = unMago {salud = salud unMago + unaCantidad}

-- 2 -- 
poder :: Mago -> Int 
poder unMago = (salud unMago + (edad unMago * length (hechizos unMago)))

daño :: Mago -> Hechizo ->  Int 
daño unMago unHechizo = salud unMago - salud (unHechizo unMago)

diferenciaDePoder :: Mago -> Mago -> Int 
diferenciaDePoder unMago otroMago = abs (poder unMago - poder otroMago)

-- 3 -- 
data Academia = Academia {
    magos :: [Mago],
    examenDeIngreso :: Mago -> Bool
}

hayMagoSinHechizosYConNombre :: Academia -> Bool
hayMagoSinHechizosYConNombre academia = any (\mago -> nombre mago == "Rincenwind" && null (hechizos mago)) (magos academia)

todosLosMagosSon :: Academia -> Bool -- ñoños
todosLosMagosSon academia = all tieneMasHechizosQueSalud (filter tieneMasDeCincuenta (magos academia))

tieneMasDeCincuenta :: Mago -> Bool
tieneMasDeCincuenta mago =  edad mago > 50 

tieneMasHechizosQueSalud :: Mago -> Bool
tieneMasHechizosQueSalud mago = length (hechizos mago) > salud mago

noPasaronElExamen :: Academia -> Int 
noPasaronElExamen academia = length . filter ( not . examenDeIngreso academia) $ magos academia 

sumatoriaMagosDiezHechizos :: Academia -> Int 
sumatoriaMagosDiezHechizos academia = sum . map edad . filter (\mago -> length (hechizos mago) > 10) . magos 

-- 4 -- 
-- maximoSegun criterio valor comparables = foldl1 (mayorSegun $ criterio valor) comparables 
-- mayorSegun evaluador comparador1 comparador2
--    | comparable1 >= evaluador comparador2  = comparable1
--    | otherwise                             = comparable2

mejorHechizoContra :: Mago -> Mago -> Hechizo 
mejorHechizoContra unMago = maximoSegun danio unMago . hechizos 

mejorOponente :: Mago -> Academia -> Mago 
mejorOponente mago academia = maximoSegun diferenciaDePoder mago (magos academia)

-- 5 -- 
noPuedeGanarle :: Mago -> Mago -> Bool 
noPuedeGanarle unMago otroMago = salud unMago == salud (foldl (flip $) unMago (hechizos otroMago))

