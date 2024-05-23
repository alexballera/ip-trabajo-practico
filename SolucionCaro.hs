module Solucion where
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
              |letraANatural c + n >=0 && letraANatural c + n <= 25 = chr (ord c + n)
              |n > 0 = chr (96 + mod (n - 25 + letraANatural c) 26)
              |otherwise = chr (ord 'a'+ mod (26 + n + letraANatural c) 26)
--testeado con numeros grandes. Preparar mas casos especiales para testear 
--EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (s:ss) n |not (esMinuscula s) = s : cifrar ss n
                |otherwise = desplazar s n:cifrar ss n
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


cantidadVecesLetra :: Char -> String -> Int
cantidadVecesLetra _ [] = 0
cantidadVecesLetra c (x:xs) |c == x = 1 + cantidadVecesLetra c xs
                            |otherwise = cantidadVecesLetra c xs

cantidadMinusculas :: String -> Int
cantidadMinusculas [] = 0
cantidadMinusculas (c:ls) |esMinuscula c = 1 + cantidadMinusculas ls
                          |otherwise = cantidadMinusculas ls

--porcentajeFrecuencia :: Char -> String -> Int -> Float
--porcentajeFrecuencia c s n |n == 0 = 0.00
 --                          |otherwise = (cantidadVecesLetra c s)/n * 100

porcentajeFrecuencia :: Char -> String -> Int -> Int
porcentajeFrecuencia c s n |n == 0 = 0
                           |otherwise = cantidadVecesLetra c s * 100
-- OJO!! me falta agregar la division por n (cantidadMinusculas s) pero tengo que ver el tema float de agregar a frecuencia, frecuenciaAux 
--y porcentajeFrecuencia
frecuenciaAux :: Char -> String -> [Int]
frecuenciaAux 'z'_ = []
frecuenciaAux c s = porcentajeFrecuencia c s (cantidadMinusculas s) : frecuenciaAux  (desplazar c 1) s

-- EJ 7
frecuencia :: String -> [Int]
frecuencia [] = []
frecuencia (s:ls )= frecuenciaAux 'a' (s:ls)

--cifradoMasFrecuenteAux :: [Float]-> (Char, Float)
--cifradoMasFrecuenteAux [] = ('a',0.0)
--cifradoMasFrecuenteAux (x:y:xs)|x>y = ( posicion x ,x)
--                               |otherwise = y: cifradoMasFrecuenteAux xs
--ver posicion relativa y dato maximo y caso base de requerirse  PEDIENTE _____________REVEERRRRRRRRRR


esDescifrado:: String -> String -> Bool
esDescifrado s1 s2 = esDescifradoAux s1 s2 0

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux _ _ 27 = False
esDescifradoAux s1 s2 n | s2 == cifrar s1 n= True
                        |otherwise = esDescifradoAux s1 s2 (n+1)


todosLosDescifradosAux :: String -> [String] -> [(String,String)]
todosLosDescifradosAux _ [] = []
todosLosDescifradosAux s ls |esDescifrado s (head ls) && s /= head ls = (s,head ls): (head ls,s): todosLosDescifradosAux s (tail ls)
                            |otherwise = todosLosDescifradosAux s (tail ls)
-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados ls = todosLosDescifradosAux (head ls) (tail ls) ++ todosLosDescifrados (tail ls)



--AgregarAClave :: String -> Int -> String
--AgregarAClave _ 0 = []
--AgregarAClave s n = head s : AgregarAClave  (tail s ) (n-1)

-- EJ 11
--expandirClave :: String -> Int -> String
--expandirClave s n =  s ++ agregarAClave s (n - length s + 1)

-- EJ 12
--cifrarVigenere :: String -> String -> String
--cifrarVigenere [] [] = []
--cifrarVigenere (c:ls) clave = (desplazar c (letraANatural (head (expandirClave clave))) : cifrarVigenere ls (tail expandir (clave))


-- EJ 13
--descifrarVigenere :: String -> String -> String
--descifrarVigenere [] [] = []
--descifrarVigenere (c:ls) clave = (desplazar c ( - letraANatural (head (espandirClave clave))) : cifrarVigenere ls (tail clave)

--listarClavesExpandidas :: Int -> [String]
--listarClavesExpandidas n (clave:ls) = (expandirClave clave n) : listarClavesExpandidas ls

--distanciaCifrado :: String -> String -> (
--distanciaCifrado (x:s1) (y:s2) = modulo ((letraANatural x)- (letraANatural y)) + distanciaCifrado s1

--listarDistanciaCifrado :: String -> [String] -> [(String,String)]
--listarDistanciaCifrado s (clave:ls) = (clave, distanciaCifrado s clave)  : listarDistanciaCifrado s ls      

--menorDistanciaCifrado :: [(String, String)]-> [String,String]
--menorDistanciaCifrado [] = []
--menorDistanciaCifrado ((clave1,d1):(clave2,d2):ls) | d1 < d2 = (clave1,d1) : menorDistanciaCifrado ls
--                                                   |otherwise = (clave2,d2) : menorDistanciaCifrado ls

-- EJ 14
--peorCifrado :: String -> [String] -> String
--peorCifrado _ [] = []
--peorCifrado s ls = fst (head (menorDistanciaCifrado (ListarDistanciaCifrado s (listarClavesExpandidas  (length s) ls ))))

--funcion a tener en cuenta si necesito:
--absoluto :: Int -> Int -> Int
--absoluto x | x<0 = -x
--           |otherwise = x

-- EJ 15
--combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
--combinacionesVigenere _ _ _ = [("hola", "b")]
