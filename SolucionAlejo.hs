{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SolucionAlejo where
    import Data.Char

    --Ejercicio 1
    esMinuscula :: Char -> Bool
    esMinuscula c = ord c >= ord 'a' && ord c <= ord 'z'
    --Ejercicio 2
    letraANatural :: Char -> Int
    letraANatural c = ord c - ord 'a'
                    

    --Ejercicio 3
    desplazar :: Char -> Int -> Char
    desplazar c 0 = c
    desplazar c n |not (esMinuscula c) = c
                  |(letraANatural c) + n >=0 && (letraANatural c) + n <= 25 = chr(ord c + n)
			      |n > 0 = chr(96 + (mod (n - 25 + letraANatural c) 26))
			      |otherwise = chr((ord 'a')+ mod (26 + n + letraANatural c) 26)

    --Ejercicio 4
    cifrar :: String -> Int -> String
    cifrar "" _ = ""
    cifrar (s:ss) n | not (esMinuscula s) = s : (cifrar ss n)
                    | otherwise = (desplazar s n) : (cifrar ss n)
    
    --Ejercicio 5
    descifrar :: String -> Int -> String
    descifrar s n = cifrar s (-n)

    --Ejercicio 6
    cifrarListaAux :: [String] -> Int -> [String]
    cifrarListaAux [] _ = []
    cifrarListaAux (s:ss) n = (cifrar s n) : (cifrarListaAux ss (n + 1))

    cifrarLista :: [String] -> [String]
    cifrarLista x = cifrarListaAux x 0

    --Ejercicio 7
    frecuencia :: String -> [Float]
    frecuencia s = frecuenciaAux s 'a'

    frecuenciaAux :: String -> Char -> [Float]
    frecuenciaAux s 'z' = [calcularPorcentaje 'z' s]
    frecuenciaAux s c = (calcularPorcentaje c s) : (frecuenciaAux s (desplazar c 1)) 

    apareceVeces :: Char -> String -> Float
    apareceVeces _ [] = 0
    apareceVeces c (x:xs) | c == x = (apareceVeces c xs) + 1
                          | otherwise = apareceVeces c xs

    calcularPorcentaje :: Char -> String -> Float
    calcularPorcentaje c s | not (esMinuscula c) = 0
                           |otherwise = ((apareceVeces c s)/fromIntegral(cantidadMinusculas (s)))*100

    cantidadMinusculas :: String -> Int
    cantidadMinusculas [] = 0
    cantidadMinusculas (c:ls) |esMinuscula c = 1 + cantidadMinusculas ls
                              |otherwise = cantidadMinusculas ls


     --Ejercicio 8
    cifradoMasFrecuente :: String -> Int -> (Char, Float)
    cifradoMasFrecuente s n = (mayorC (cifrar s n), mayorN (frecuencia (cifrar s n)))


    mayorN :: [Float] -> Float
    mayorN [x] = x
    mayorN (x:xs) | x >= (head xs) = mayorN (x:(tail xs)) 
                  | otherwise = mayorN xs

    mayorC :: String -> Char
    mayorC [x] = x 
    mayorC (x:xs) | mayorN (frecuencia (x:xs)) == calcularPorcentaje x (x:xs) = x
                  | otherwise = mayorC xs

 
    --Ejercicio 9

    esDescifrado :: String -> String -> Bool
    esDescifrado s1 s2 = esDescifradoAux s1 s2 0

    esDescifradoAux :: String -> String -> Int-> Bool
    esDescifradoAux _ _ 27 = False
    esDescifradoAux s1 s2 n | s2 == cifrar s1 n = True
                            | otherwise = esDescifradoAux s1 s2 (n + 1)

    -- EJ 10
    todosLosDescifradosAux :: String -> [String] -> [(String,String)]
    todosLosDescifradosAux _ [] = []
    todosLosDescifradosAux s ls |(esDescifrado s (head ls)) && (s /= head ls) = (s,head ls): (head ls,s): todosLosDescifradosAux s (tail ls)
                            |otherwise = todosLosDescifradosAux s (tail ls)

    todosLosDescifrados :: [String] -> [(String, String)]
    todosLosDescifrados [] = []
    todosLosDescifrados ls = (todosLosDescifradosAux (head ls) (tail ls)) ++ todosLosDescifrados (tail ls)


    --Ejercicio 11
    
    expandirClave :: String -> Int -> String
    expandirClave _ 0 = ""
    expandirClave (x:xs) n | length (x:xs) > n = x : (expandirClave xs (n - 1))
                           | otherwise = (x:xs) ++ (expandirClave (x:xs) (n - (length (x:xs))))
  



    --Ejercicio 12
    cifrarVigenere :: String -> String -> String
    cifrarVigenere [] _ = ""
    cifrarVigenere (s:ss) (c:cs) = (desplazar s (letraANatural c)) : (cifrarVigenere ss (tail (expandirClave (c:cs) (length (s:ss))))) 

    --Ejercicio 13
    descifrarVigenere :: String -> String -> String
    descifrarVigenere [] [] = []
    descifrarVigenere (x:xs) (c:cs) = (desplazar x (-(letraANatural c))):(descifrarVigenere xs (tail (expandirClave (c:cs) (length (x:xs)))))

    --Ejercicio 14

    peorCifrado :: String -> [String] -> String
    peorCifrado _ [x] = x
    peorCifrado s (x:y:xs) | distancia s (cifrarVigenere s x) > distancia s (cifrarVigenere s y) = peorCifrado s (y:xs)
                           | otherwise = peorCifrado s (x:xs) 

    distancia :: String -> String -> Int
    distancia [] _ = 0
    distancia (s:ss) (x:xs) = (absoluto((letraANatural s) - (letraANatural x))) + (distancia ss xs) 

    absoluto :: Int -> Int
    absoluto x | x < 0 = -x
               | otherwise = x 

    --Ejercicio 15
    combinacionesVigenereAux :: String -> [String] -> String -> [(String, String)]
    combinacionesVigenereAux _ [] _ = []
    combinacionesVigenereAux x (y:ys) cifrado | cifrarVigenere x y == cifrado = (x, y) : (combinacionesVigenereAux x ys cifrado)
                                              | otherwise = combinacionesVigenereAux x ys cifrado

    combinacionesVigenere :: [String] -> [String] -> String -> [(String, String)]
    combinacionesVigenere [] _ _ = []
    combinacionesVigenere (x:xs) y cifrado = (combinacionesVigenereAux x y cifrado) ++ (combinacionesVigenere xs y cifrado)