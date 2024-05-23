{-# OPTIONS_GHC -Wno-unrecognised-pragmas #-}
{-# HLINT ignore "Redundant bracket" #-}
module SolucionAlejo where
    import Data.Char

    --Ejercicio 1
    esMinuscula :: Char -> Bool
    esMinuscula c = ord c >= ord 'a' && ord c <= ord 'z'
    --Ejercicio 2
    letraANatural :: Char -> Int
    letraANatural c | esMinuscula c = ord c - ord 'a'
                    | otherwise = -1

    --Ejercicio 3
    desplazar :: Char -> Int -> Char
    desplazar c n | not (esMinuscula c) = c 
                  | ((ord c) + n) <= 122 &&  ((ord c) + n) >= 97 = chr ((ord c) + n)
                  | n >= 0 =  chr (((ord c) + n) - 26)
                  | otherwise = chr (((ord c) + n) + 26)

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

    len :: String -> Float
    len [] = 0
    len (x:xs) = (len xs) + 1

    calcularPorcentaje :: Char -> String -> Float
    calcularPorcentaje c s | not (esMinuscula c) = 0
                           |otherwise = ((apareceVeces c s)/len (s))*100

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



    --Ejercicio 11
    expandirClave :: String -> Int -> String
    expandirClave _ 0 = ""
    expandirClave (x:xs) n | lenght (x:xs) > n = x : (expandirClave xs (n - 1))
                           | otherwise = (x:xs) ++ (expandirClave (x:xs) (n - (lenght (x:xs)))) 


    --Ejercicio 12
    cifrarVigenere :: String -> String -> String
    cifrarVigenere [] _ = ""
    cifrarVigenere (s:ss) (c:cs) = (desplazar s (letraANatural c)) : (cifrarVigenere ss (tail (expandirClave (c:cs) (length (s:ss))))) 

    --Ejercicio 13
    descifrarVigenere :: String -> String -> String
    desscifrarVigenere [] _ = ""
    descifrarVigenere (s:ss) (c:cs) = (desplazar s (-(letraANatural c))) : (descifrarVigenere ss (tail (expandirClave (c:cs) (length (s:ss))))) 
