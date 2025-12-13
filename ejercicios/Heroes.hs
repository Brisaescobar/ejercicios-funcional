import Text.Show.Functions
import Data.List

-- punto 1 -- 
data Heroe = Heroe {
    epiteto        :: String, 
    reconocimiento :: Int, 
    artefactos     :: [Artefacto], 
    tareas         :: [Tarea]
} deriving (Show)

data Artefacto = Artefacto {
    nombre :: String, 
    rareza :: Int
} deriving (Show)

-- punto 2 --
pasarALaHistoria :: Heroe -> Heroe
pasarALaHistoria unheroe 
 | reconocimiento unheroe > 1000 = cambiarEpiteto "El militico" unheroe
 | reconocimiento unheroe >= 500 = cambiarEpiteto "El magnifico" . agregarArtefacto lanzaDelOlimpo $ unheroe 
 | reconocimiento unheroe > 100 = cambiarEpiteto  "Hoplita" . agregarArtefacto xiphos $ unheroe
 | otherwise              = unheroe

cambiarEpiteto :: String -> Heroe -> Heroe 
cambiarEpiteto unepiteto unheroe = unheroe {epiteto = unepiteto} 
 
agregarArtefacto :: Artefacto -> Heroe -> Heroe
agregarArtefacto unartefacto  = cambiarArtefactos (unartefacto :)  -- saco un heroe

cambiarArtefactos :: ([Artefacto] -> [Artefacto]) -> Heroe -> Heroe
cambiarArtefactos unaFuncion unheroe = unheroe { artefactos = unaFuncion $ artefactos unheroe }

lanzaDelOlimpo :: Artefacto
lanzaDelOlimpo = Artefacto "Lanza del olimpo" 100 

xiphos :: Artefacto
xiphos = Artefacto "Xiphos" 50 

-- punto 3 -- 
type Tarea = Heroe -> Heroe 

-- primer tarea -- 
encontrarUnArtefacto :: Artefacto -> Tarea 
encontrarUnArtefacto unartefacto = ganarReconocimiento (rareza unartefacto) . agregarArtefacto unartefacto

ganarReconocimiento :: Int -> Heroe -> Heroe 
ganarReconocimiento cantidad unheroe = unheroe {reconocimiento = reconocimiento unheroe + cantidad}

-- segunda tarea -- 
escalarElOlimpo :: Tarea 
escalarElOlimpo = agregarArtefacto elRelampagoDeZeus . desecharArtefactos . triplicarRareza . ganarReconocimiento 500

triplicarRareza :: Tarea 
triplicarRareza = cambiarArtefactos (map triplicarRarezaArtefactos)

triplicarRarezaArtefactos :: Artefacto -> Artefacto 
triplicarRarezaArtefactos unartefacto = unartefacto {rareza = rareza unartefacto * 3 }

elRelampagoDeZeus :: Artefacto
elRelampagoDeZeus = Artefacto "El relampago de Zeus" 500

desecharArtefactos :: Tarea 
desecharArtefactos = cambiarArtefactos (filter (not . esComun))

esComun :: Artefacto -> Bool
esComun unartefacto = rareza unartefacto < 1000

-- tercer tarea -- 
ayudarACruzar :: Int -> Tarea 
ayudarACruzar cuadras = cambiarEpiteto ("Gros" ++ replicate cuadras 'o') -- replicate Int -> a -> [a]

-- cuarta tarea --
matarBestia :: Bestia -> Tarea 
matarBestia unabestia unheroe 
    | debilidad unabestia unheroe = cambiarEpiteto ("El asesino de " ++ nombrebestia unabestia) unheroe
    | otherwise = cambiarEpiteto "El cobarde" . cambiarArtefactos (drop 1 ) $ unheroe -- drop 2 [1 2 3 4 ] = [3 4]

data Bestia = Bestia {
    nombrebestia :: String,
    debilidad :: Debilidad
} deriving (Show)

type Debilidad = Heroe -> Bool

-- punto 4 -- 
heracles :: Heroe 
heracles = Heroe "Guardian del olimpo" 700 [pistolaRara, elRelampagoDeZeus] [matarBestia matarLeon]

pistolaRara :: Artefacto
pistolaRara = Artefacto "Fierro de la antigua grecia" 1000 

-- punto 5 -- 
matarLeon :: Bestia 
matarLeon = Bestia "Leon de Nemea" ((>=20) . length . epiteto) 

--punto 6 -- 
hacerTarea :: Heroe -> Tarea -> Heroe 
hacerTarea unheroe unatarea = agregarTarea unatarea (unatarea unheroe)

agregarTarea :: Tarea -> Heroe -> Heroe 
agregarTarea unatarea unheroe = unheroe {tareas = unatarea : tareas unheroe} --suma la tarea a la lista

-- punto 7 -- 
presumir :: Heroe -> Heroe -> (Heroe , Heroe) -- devuelve una tupla 
presumir heroe1 heroe2 
    | reconocimiento heroe1 > reconocimiento heroe2 = (heroe1 , heroe2)
    | reconocimiento heroe1 < reconocimiento heroe2 = (heroe2 , heroe1)
    | sumDeRareza (artefactos heroe1)  > sumDeRareza (artefactos heroe2) = (heroe1 , heroe2)
    | sumDeRareza (artefactos heroe1 ) < sumDeRareza (artefactos heroe2) = (heroe2 , heroe1)
 -- | otherwise = presumir (realizarLabor1 (tareas heroe2) heroe1) (realizarLabor1 (tareas heroe1) heroe2)


-- realizarTareasDe :: Heroe -> Heroe -> Heroe
-- realizarTareasDe unheroe  = realizarLabor1 hacerTarea  (tareas unheroe)--saco otro heroe 

sumDeRareza :: [Artefacto] -> Int 
sumDeRareza = sum . map rareza 

-- punto 8 --
-- respuesta: queda en loop infinito 

-- punto 9 -- 
type Labor = Tarea -> Heroe -> Heroe 

-- realizarLabor :: Labor -> Heroe -> Heroe 
-- realizarLabor unastareas unheroe = foldl (flip hacerTarea) unheroe unastareas

realizarLabor1 :: Labor -> [Tarea] -> Heroe -> Heroe
realizarLabor1 labor tareas heroe = foldl (flip labor) heroe tareas 

-- punto 10 --
-- respuesta: no va a devolver un heroe 
