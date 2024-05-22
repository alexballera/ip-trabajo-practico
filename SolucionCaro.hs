module SolucionCaro where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {}
-- Integrante1: { 27355966,Banegas Carolina Alejandra}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= ord 'a' && ord c <= ord 'z'
--OJO!! Revisar. Faltó en programado ALEJO, el menor o igual ord 'z'. Ver si queda mejor el ord 'a', que el 97. 

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - ord 'a'
--OK. IDENTICO a lo programado por ALEJO, excepto el ord 'a', que veremos si conviene dejarlo asi porque se lee mejor. 
desplazar :: Char -> Int -> Char
desplazar c 0 = c
desplazar c n |not (esMinuscula c) = c
              |(letraANatural c) + n >=0 && (letraANatural c) + n <= 25 = chr (ord c + n)
              |n > 0 = chr (96 + (mod (n - 25 + letraANatural c) 26))
              |otherwise = chr ((ord 'a')+ mod (26 + n + letraANatural c) 26)

--testeado con numeros grandes. Preparar mas casos especiales para testear 
--EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (s:ss) n |not (esMinuscula s) = s : cifrar ss n
                |otherwise = (desplazar s n):cifrar ss n
--OK. IDENTICO a lo programado por ALEJO. 

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar s n = cifrar s (-n)
--OK. IDENTICO a lo programado por ALEJO


cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (palabra:ls) n = cifrar palabra (n-(length ls + 1)): cifrarListaAux ls n

--version ALEJO: tiene un calculo menos la de Alejo. Conviene usar la suya.
--cifrarListaAux :: [String] -> Int -> String 
--cifrarListaAux [] _ = []
--cifrarListaAux (s:ss) n = (cifrar s n) : (cifrarListaAux ss (n + 1))
--cifrarLista :: [String]->[String]
--cifrarLista x = cifrarListaAux x 0

-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista (palabra:ls) = cifrarListaAux (palabra:ls) (length (palabra:ls))

