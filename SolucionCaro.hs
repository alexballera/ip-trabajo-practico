module SolucionCaro where
import Data.Char
-- No se permite agrear nuevos imports
-- Sólo está permitido usar estas funciones:
-- https://campus.exactas.uba.ar/pluginfile.php/557895/mod_resource/content/1/validas_tp.pdf


-- Completar!
-- Nombre de grupo: {Exactamentes}
-- Integrante1: { 27355966,Banegas Carolina Alejandra}
-- Integrante2: { DNI2,apellidoYNombre2}
-- Integrante3: { DNI3,apellidoYNombre3}
-- Integrante4: { DNI4,apellidoYNombre4}
-- Integrantes que abandonaron la materia: {En caso que haya abandonado la materia algún
                        -- integrante, completar con los dni y apellidos, sino dejar vacío}

-- EJ 1
esMinuscula :: Char -> Bool
esMinuscula c = ord c >= ord 'a' && ord c <= ord 'z'

-- EJ 2
letraANatural :: Char -> Int
letraANatural c = ord c - (ord 'a')

--EJ 3
desplazar :: Char -> Int -> Char
desplazar c 0 = c
desplazar c n |not (esMinuscula c) = c
              |(letraANatural c) + n >=0 && (letraANatural c) + n <= 25 = chr(ord c + n)
			  |n > 0 = chr(96 + (mod (n - 25 + letraANatural c) 26))
			  |otherwise = chr((ord 'a')+ mod (26 + n + letraANatural c) 26)

--EJ 4
cifrar :: String -> Int -> String
cifrar [] _ = []
cifrar (s:ss) n |not (esMinuscula s) = s : cifrar ss n
                |otherwise = (desplazar s n):cifrar ss n

-- EJ 5
descifrar :: String -> Int -> String
descifrar [] _ = []
descifrar s n = cifrar s (-n)


cifrarListaAux :: [String] -> Int -> [String]
cifrarListaAux [] _ = []
cifrarListaAux (palabra:ls) n = cifrar palabra (n-(length ls + 1)): cifrarListaAux ls n


-- EJ 6
cifrarLista :: [String] -> [String]
cifrarLista [] = []
cifrarLista (palabra:ls) = cifrarListaAux (palabra:ls) (length (palabra:ls))


cantidadVecesLetra :: Char -> String -> Int
cantidadVecesLetra _ [] = 0
cantidadVecesLetra c (x:xs) |c == x = 1 + (cantidadVecesLetra c xs)
                            |otherwise = (cantidadVecesLetra c xs)

cantidadMinusculas :: String -> Int
cantidadMinusculas [] = 0
cantidadMinusculas (c:ls) |esMinuscula c = 1 + cantidadMinusculas ls
                          |otherwise = cantidadMinusculas ls
						  
dividir :: Int -> Int -> Float
dividir a b = fromIntegral a / fromIntegral b

porcentajeFrecuencia :: Char -> String -> Int -> Float
porcentajeFrecuencia c s n |(n == 0 ) = 0
                           |otherwise = (dividir (cantidadVecesLetra c s) n )* 100

frecuenciaAux :: Char -> String -> [Float]
frecuenciaAux 'z' s = [porcentajeFrecuencia 'z' s (cantidadMinusculas s)]
frecuenciaAux c s = ( porcentajeFrecuencia c s (cantidadMinusculas s) : frecuenciaAux  (desplazar c 1) s )
                           
-- EJ 7
frecuencia :: String -> [Float]
frecuencia s = frecuenciaAux 'a' s 

--EJ 8

--cifradoMasFrecuenteAux :: [Float]-> (Char, Float)
--cifradoMasFrecuenteAux [] = ('a',0.0)
--cifradoMasFrecuenteAux (x:y:xs)|x>y = ( posicion x ,x)
--                               |otherwise = y: cifradoMasFrecuenteAux xs
--ver posicion relativa y dato maximo y caso base de requerirse


--EJ 9
esDescifrado:: String -> String -> Bool				 
esDescifrado s1 s2 = esDescifradoAux s1 s2 0

esDescifradoAux :: String -> String -> Int -> Bool
esDescifradoAux _ _ 27 = False
esDescifradoAux s1 s2 n | (s2 == (cifrar s1 n) )= True
                        |otherwise = esDescifradoAux s1 s2 (n+1)


todosLosDescifradosAux :: String -> [String] -> [(String,String)]
todosLosDescifradosAux _ [] = []
todosLosDescifradosAux s ls |(esDescifrado s (head ls)) && (s /= head ls) = (s,head ls): (head ls,s): todosLosDescifradosAux s (tail ls)
                            |otherwise = todosLosDescifradosAux s (tail ls)
-- EJ 10
todosLosDescifrados :: [String] -> [(String, String)]
todosLosDescifrados [] = []
todosLosDescifrados ls = (todosLosDescifradosAux (head ls) (tail ls)) ++ todosLosDescifrados (tail ls)


agregarAClave :: String -> Int -> String
agregarAClave _ 0 = []
agregarAClave s n = (head s ): agregarAClave  (tail s) (n-1) 

repetirClave :: String -> Int -> String
repetirClave _ 0 = []
repetirClave s n = s ++ repetirClave s (n-1)

-- EJ 11
expandirClave :: String -> Int -> String
expandirClave s n | (n < length s ) = (agregarAClave s n )
                  |otherwise = (repetirClave s (div n (length s)) ) ++ (  agregarAClave s (mod n (length s))  ) 


cifrarVigenere :: String -> String -> String
cifrarVigenere [] _ = []
cifrarVigenere (x:xs) (c:cs) =(desplazar x (letraANatural c)): cifrarVigenere xs (tail (expandirClave (c:cs) (length (x:xs))))


-- EJ 13
descifrarVigenere :: String -> String -> String
descifrarVigenere [] [] = []
descifrarVigenere (x:xs) (c:cs) = (desplazar x (-(letraANatural c))):(descifrarVigenere xs (tail (expandirClave (c:cs) (length (x:xs)))))

absoluto :: Int -> Int
absoluto x | x < 0 = - x 
           |otherwise = x

distanciaCifrado :: String -> String -> Int
distanciaCifrado [] _ = 0
distanciaCifrado (x:s1) (y:s2) = absoluto ((letraANatural x) - (letraANatural y)) + distanciaCifrado s1 s2

listarCifradoV :: String -> [String] -> [(String,String)]
listarCifradoV _ [] = []
listarCifradoV s (c:cs) = ( cifrarVigenere s (expandirClave c (length s)),c) : (listarCifradoV s cs)      
 
listarDistanciaCifrado :: String -> [(String,String)]-> [(String,Int)]
listarDistanciaCifrado _ [] = []                                          
listarDistanciaCifrado s (c:cs)= (snd(c), distanciaCifrado (fst(c)) s ): (listarDistanciaCifrado s cs)

menorDistanciaCifradoV :: [(String,Int)] -> [(String,Int)]
menorDistanciaCifradoV [] = []
menorDistanciaCifradoV (c1:c2:cs) | ( snd(c1) < snd(c2) ) = c1 : (menorDistanciaCifradoV cs)
                                  | otherwise = c2 : (menorDistanciaCifradoV cs)

-- EJ 14
peorCifrado :: String -> [String] -> String
peorCifrado _ [] = []
peorCifrado s cs = fst(head (menorDistanciaCifradoV (listarDistanciaCifrado s (listarCifradoV s cs))))

  
listarPosiblesDescifrados :: [String] -> String -> [(String,String)]
listarPosiblesDescifrados [] _ = []
listarPosiblesDescifrados (c:cs) s = (descifrarVigenere s (expandirClave c (length s)),c): (listarPosiblesDescifrados cs s)

listarMensajes :: [(String,String)] -> String-> [(String,String)]
listarMensajes [] _ =[]
listarMensajes (d:dc) m | ((fst (d)) == m) = (m,snd(d)) : (listarMensajes dc m)
                        | otherwise = (listarMensajes dc m)
 
						 							 
-- EJ 15
combinacionesVigenere :: [String] -> [String] -> String -> [(String,String)]
combinacionesVigenere [] _ _ = []
combinacionesVigenere (m:ms) cs s = (listarMensajes (listarPosiblesDescifrados cs s) m) ++ (combinacionesVigenere ms cs s)