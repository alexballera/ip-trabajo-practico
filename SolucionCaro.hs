module Solucion where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Exactamentes}
-- Integrante1: { 27.355.966,Banegas Carolina Alejandra}
-- Integrante2: { 43447394,Mazza Alejo}
-- Integrante3: { 8983523,Ballera Alexander}
-- Integrante4: { }
-- Integrantes que abandonaron la materia: {39757107, Flores Ignacio Enrique}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= ord 'a' && ord c <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - ord 'a'

-- EJ 3
-- uso de MOD para trabajar con restos congruencia modulo 26 para casos desplazamiento con rotación al principio o al final abecedario
desplazar :: Char -> Int -> Char
desplazar c 0 = c
desplazar c n |not (esMinuscula c) = c
              |(letraANatural c) + n >=0 && (letraANatural c) + n <= 25 = chr(ord c + n)      --caso desplazamiento sin rotación
              |n > 0 = chr(96 + (mod (n - 25 + letraANatural c) 26))                          --caso desplazamiento con rotación y n positivo
              |otherwise = chr((ord 'a')+ mod (26 + n + letraANatural c) 26)                  --caso desplazamiento con rotación y n negativo

-- EJ 4
cifrar :: String -> Int -> String
cifrar "" _ = ""
cifrar (s:ss) n | not (esMinuscula s) = s : (cifrar ss n)
                | otherwise = (desplazar s n) : (cifrar ss n)
    
-- EJ 5
descifrar :: String -> Int -> String
descifrar s n = cifrar s (-n)

-- EJ 6
cifrarLista :: [String] -> [String]  
cifrarLista x = cifrarListaAux x 0    

                      --función auxiliar--

cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (s:ss) n = (cifrar s n) : (cifrarListaAux ss (n + 1))

-- EJ 7

frecuencia :: String -> [Float]                                                                  
frecuencia s = frecuenciaAux s 'a'

                      --funciones auxiliares--

frecuenciaAux :: String -> Char -> [Float]          --recorre abecedario y por cada letra, calcula porcentaje frecuencia 
frecuenciaAux s 'z' = [calcularPorcentaje 'z' s]
frecuenciaAux s c = (calcularPorcentaje c s) : (frecuenciaAux s (desplazar c 1))

apareceVeces :: Char -> String -> Float         --cantidad veces una letra aparece en una palabra      
apareceVeces _ [] = 0
apareceVeces c (x:xs) | c == x = (apareceVeces c xs) + 1
                      |otherwise = apareceVeces c xs

cantidadMinusculas :: String -> Int
cantidadMinusculas [] = 0
cantidadMinusculas (c:ls) |esMinuscula c = 1 + cantidadMinusculas ls
                          |otherwise = cantidadMinusculas ls

calcularPorcentaje :: Char -> String -> Float                  --porcentaje aparición un letra minuscula sobre total letras minusculas de una palabra
calcularPorcentaje c s |((cantidadMinusculas s )== 0) = 0
                       |otherwise = ((apareceVeces c s)/fromIntegral(cantidadMinusculas (s)))*100   

-- Ej 8

cifradoMasFrecuente :: String -> Int -> (Char, Float) 
cifradoMasFrecuente s n = (mayorC (cifrar s n), mayorN (frecuencia (cifrar s n)))  -- par :letra de mayor frecuencia y su frecuencia (en una palabra)

                         --funciones auxiliares--
						 
mayorN :: [Float] -> Float   --frecuencia letra de mayor frecuencia aparición en una palabra
mayorN [x] = x
mayorN (x:xs) | x >= (head xs) = mayorN (x:(tail xs)) 
              | otherwise = mayorN xs

mayorC :: String -> Char     --letra de mayor frecuencia aparición en una palabra
mayorC [x] = x 
mayorC (x:xs) | mayorN (frecuencia (x:xs)) == calcularPorcentaje x (x:xs) = x
              |otherwise = mayorC xs

-- EJ 9
esDescifrado :: String -> String -> Bool
esDescifrado s1 s2 = esDescifradoAux s1 s2 0

                           --funcion auxiliar--      

esDescifradoAux :: String -> String -> Int-> Bool
esDescifradoAux _ _ 27 = False
esDescifradoAux s1 s2 n | s2 == cifrar s1 n = True
                        | otherwise = esDescifradoAux s1 s2 (n + 1)

-- EJ 10

todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados ls = (todosLosDescifradosAux (head ls) (tail ls)) ++ todosLosDescifrados (tail ls) 

                          --función auxiliar--

todosLosDescifradosAux :: String -> [String] -> [(String,String)]        
todosLosDescifradosAux _ [] = []
todosLosDescifradosAux s ls |(esDescifrado s (head ls)) && (s /= head ls) = (s,head ls): (head ls,s): todosLosDescifradosAux s (tail ls)
                            |otherwise = todosLosDescifradosAux s (tail ls)

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave _ 0 = ""
expandirClave (x:xs) n | length (x:xs) > n = x : (expandirClave xs (n - 1))                  --caso n menor a longitud clave (trunca clave)
                       | otherwise = (x:xs) ++ (expandirClave (x:xs) (n - (length (x:xs))))  --caso n mayor a longitud clave (añade letras faltantes a clave)
  

-- EJ 12
cifrarVigenere :: String -> String -> String    --recorre letra x letra cifrando Vigenere (lo que requiere expandir la clave para el cifrado)
cifrarVigenere [] _ = ""
cifrarVigenere (s:ss) (c:cs) = (desplazar s (letraANatural c)) : (cifrarVigenere ss (tail (expandirClave (c:cs) (length (s:ss))))) 


-- EJ 13
descifrarVigenere :: String -> String -> String   --recorre letra x letra descifrando Vigenere (lo que requiere expandir la clave para el descifrado)
descifrarVigenere [] [] = []                                  
descifrarVigenere (x:xs) (c:cs) = (desplazar x (-(letraANatural c))):(descifrarVigenere xs (tail (expandirClave (c:cs) (length (x:xs)))))


-- EJ 14
peorCifrado :: String -> [String] -> String   
peorCifrado _ [x] = x
peorCifrado s (x:y:xs) | distancia s (cifrarVigenere s x) > distancia s (cifrarVigenere s y) = peorCifrado s (y:xs)
                       | otherwise = peorCifrado s (x:xs) 
 
                            --funciones auxiliares--

distancia :: String -> String -> Int   --calcula sumatoria valores absolutos distancia por cada letra palabra cifrada y letra clave en mismo orden
distancia [] _ = 0
distancia (s:ss) (x:xs) = (absoluto((letraANatural s) - (letraANatural x))) + (distancia ss xs) 

absoluto :: Int -> Int
absoluto x | x < 0 = -x
           | otherwise = x 


-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]   --recorre mensaje (listado palabras) y usa función auxiliar con c/palabra
combinacionesVigenere [] _ _ = []
combinacionesVigenere (x:xs) y cifrado = (combinacionesVigenereAux x y cifrado) ++ (combinacionesVigenere xs y cifrado)

                           --funcion auxiliar--

combinacionesVigenereAux :: String -> [String] -> String -> [(String, String)]  --recorre listado claves, y contrasta cifradoVigenere (palabra-clave) con cifrado
combinacionesVigenereAux _ [] _ = []
combinacionesVigenereAux x (y:ys) cifrado | cifrarVigenere x y == cifrado = (x, y) : (combinacionesVigenereAux x ys cifrado)
                                          | otherwise = combinacionesVigenereAux x ys cifrado
