module TP where
    import Data.Char

    --Ejercicio 1
    esMinuscula :: Char -> Bool
    esMinuscula c = (ord c) >= 97

    --Ejercicio 2
    letraANatural :: Char -> Int
    letraANatural c = (ord c) - 97

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

