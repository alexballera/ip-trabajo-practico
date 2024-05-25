import Test.HUnit
import SolucionAlejo
import Data.List
-- No está permitido agregar nuevos imports.

runCatedraTests :: IO Counts
runCatedraTests = runTestTT allTests

allTests :: Test
allTests = test [
    "esMinuscula" ~: testsEjesMinuscula,
    "letraANatural" ~: testsEjletraANatural,
    "desplazar" ~: testsEjdesplazar,
    "cifrar" ~: testsEjcifrar,
    "descifrar" ~: testsEjdescifrar,
    "cifrarLista" ~: testsEjcifrarLista,
    "frecuencia" ~: testsEjfrecuencia,
    "cifradoMasFrecuente" ~: testsEjcifradoMasFrecuente,
    "esDescifrado" ~: testsEjesDescifrado,
    "todosLosDescifrados" ~: testsEjtodosLosDescifrados,
    "expandirClave" ~: testsEjexpandirClave,
    "cifrarVigenere" ~: testsEjcifrarVigenere,
    "descifrarVigenere" ~: testsEjdescifrarVigenere,
    "peorCifrado" ~: testsEjpeorCifrado,
    "combinacionesVigenere" ~: testsEjcombinacionesVigenere
    ]

-- EJ 1
testsEjesMinuscula :: Test
testsEjesMinuscula = test [
  "esMinuscula 'd'" ~: esMinuscula 'd' ~?= True,
  "esMinuscula 'a'" ~: esMinuscula 'a' ~=? True,
  "esMinuscula 'b'" ~: esMinuscula 'a' ~=? True,
  "esMinuscula 'c'" ~: esMinuscula 'a' ~=? True,
  "esMinuscula 'A'" ~: esMinuscula 'A' ~=? False,
  "esMinuscula 'ñ'" ~: esMinuscula 'ñ' ~=? False,
  "esMinuscula 'á'" ~: esMinuscula 'á' ~=? False,
  "esMinuscula 'é'" ~: esMinuscula 'é' ~=? False,
  "esMinuscula '1'" ~: esMinuscula '1' ~=? False,
  "esMinuscula '%'" ~: esMinuscula '%' ~=? False,
  "esMinuscula 'n'" ~: esMinuscula 'n' ~=? True
 ]

-- EJ 2
testsEjletraANatural :: Test
testsEjletraANatural = test [
  "letraANatural 'b'" ~:  letraANatural 'b' ~?= 1,
  "letraANatural 'a'" ~: letraANatural 'a' ~=? 0,
  "letraANatural 'b'" ~: letraANatural 'b' ~=? 1,
  "letraANatural 'c'" ~: letraANatural 'c' ~=? 2,
  "letraANatural 'd'" ~: letraANatural 'd' ~=? 3,
  "letraANatural 'e'" ~: letraANatural 'e' ~=? 4,
  "letraANatural 'f'" ~: letraANatural 'f' ~=? 5,
  "letraANatural 'w'" ~: letraANatural 'w' ~=? 22,
  "letraANatural 'x'" ~: letraANatural 'x' ~=? 23,
  "letraANatural 'y'" ~: letraANatural 'y' ~=? 24,
  "letraANatural 'z'" ~: letraANatural 'z' ~=? 25
 ]

-- EJ 3
testsEjdesplazar :: Test
testsEjdesplazar = test [
  "desplazar 'a' 3" ~: desplazar 'a' 3 ~=? 'd',
  "desplazar 'b' 2" ~: desplazar 'b' 2 ~=? 'd',
  "desplazar 'a' 26" ~: desplazar 'a' 26 ~=? 'a',
  "desplazar 'a' 27" ~: desplazar 'a' 27 ~=? 'b',
  "desplazar 'a' -1" ~: desplazar 'a' (-1) ~=? 'z',
  "desplazar 'a' -2" ~: desplazar 'a' (-2) ~=? 'y',
  "desplazar 'c' -1" ~: desplazar 'c' (-1) ~=? 'b',
  "desplazar 'd' -2" ~: desplazar 'd' (-2) ~=? 'b',
  "desplazar 'a' 1000" ~: desplazar 'a' 1000 ~=? 'm',
  "desplazar 'a' -1000" ~: desplazar 'a' (-1000) ~=? 'o',
  "desplazar 'e' 5" ~: desplazar 'e' 5 ~=? 'j',
  "desplazar 'f'" ~: desplazar 'f' 1 ~=? 'g',
  "desplazar 'w'" ~: desplazar 'A' 1 ~=? 'A',
  "desplazar 'x'" ~: desplazar 'B' 1 ~=? 'B',
  "desplazar 'y'" ~: desplazar 'C' 1 ~=? 'C',
  "desplazar 'z'" ~: desplazar 'D' 1 ~=? 'D',
  "desplazar 'A'" ~: desplazar 'E' 1 ~=? 'E',
  "desplazar '%'" ~: desplazar '%' 3 ~=? '%'
  ]

-- EJ 4
testsEjcifrar :: Test
testsEjcifrar = test [
  cifrar "computacion" 3 ~?= "frpsxwdflrq",
  cifrar "computacion" 9 ~?= "lxvydcjlrxw",
  cifrar "argentina" 19 ~?= "tkzxgmbgt",
  cifrar "argentina" (-19) ~?= "hynluapuh",
  cifrar "carolina" (1001) ~?= "pnebyvan"
 ]

-- EJ 5
testsEjdescifrar :: Test
testsEjdescifrar = test [
  descifrar "frpsxwdflrq" 3 ~?= "computacion",
  descifrar "lxvydcjlrxw" 9 ~?= "computacion",
  descifrar "tkzxgmbgt" 19 ~?= "argentina",
  descifrar "hynluapuh" (-19) ~?= "argentina",
  descifrar "pnebyvan" 1001 ~?= "carolina"
 ]

-- EJ 6
testsEjcifrarLista :: Test
testsEjcifrarLista = test [
  cifrarLista ["compu", "labo", "intro"] ~?= ["compu", "mbcp", "kpvtq"],
  cifrarLista ["computadora", "labos", "introd"] ~?= ["computadora","mbcpt","kpvtqf"],
  cifrarLista [] ~?= [],
  cifrarLista ["a"]~?= ["a"],
  cifrarLista ["mi compu", "el gato", "mi casa"]~?= ["mi compu","fm hbup","ok ecuc"],
  cifrarLista ["a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a","a"] ~?= ["a","b","c","d","e","f","g","h","i","j","k","l","m","n","o","p","q","r","s","t","u","v","w","x","y","z","a","b"],
  cifrarLista ["ABCDEF", "A", "%&/(/&%"]~?= ["ABCDEF", "A", "%&/(/&%"]
 ]

-- EJ 7
testsEjfrecuencia :: Test
testsEjfrecuencia = test [
  expectlistProximity (frecuencia "taller") [16.666668,0.0,0.0,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0,33.333336,0.0,0.0,0.0,0.0,0.0,16.666668,0.0,16.666668,0.0,0.0,0.0,0.0,0.0,0.0],
  expectlistProximity (frecuencia "TRAMPA") [0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0],
  expectlistProximity (frecuencia "abcdefghijklmnopqrstuvwxyz")[3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154,3.846154],
  expectlistProximity (frecuencia "aaaaaa") [100.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0,0.0]
 ]

-- EJ 8
testsEjcifradoMasFrecuente :: Test
testsEjcifradoMasFrecuente = test [
  cifradoMasFrecuente "taller" 3 ~?= ('o', 33.333336),
  cifradoMasFrecuente "amalgama" 0  ~?= ('a',50.0),
  cifradoMasFrecuente "t" 8 ~?= ('b',100.0),
  cifradoMasFrecuente "eXCELENTE" 3 ~?= ('h',100.0),
  cifradoMasFrecuente "aabbccddee" 4 ~?= ('e',20.0)
 ]

-- EJ 9
testsEjesDescifrado :: Test
testsEjesDescifrado = test [
  esDescifrado "taller" "compu" ~?= False,
  esDescifrado "compu" "compu" ~?= True,
  esDescifrado "TTTT" "TTTT" ~?= True,
  esDescifrado "TTTT" "AAAA" ~?= False,
  esDescifrado "ab" "abcde" ~?= False
 ]

-- EJ 10
testsEjtodosLosDescifrados :: Test
testsEjtodosLosDescifrados = test [
  todosLosDescifrados ["compu", "frpsx", "mywza"] ~?= [("compu", "frpsx"), ("frpsx", "compu")],
  todosLosDescifrados ["a","b","c","d"] ~?= [("a","b"),("b","a"),("a","c"),("c","a"),("a","d"),("d","a"),("b","c"),("c","b"),("b","d"),("d","b"),("c","d"),("d","c")],
  todosLosDescifrados ["compu", "compu"] ~?= [],
  todosLosDescifrados [] ~?= []
 ]

-- EJ 11
testsEjexpandirClave :: Test
testsEjexpandirClave = test [
  expandirClave "compu" 8 ~?= "compucom",
  expandirClave "compu" 1 ~?= "c",
  expandirClave "a" 20 ~?= "aaaaaaaaaaaaaaaaaaaa",
  expandirClave "argentina" 50 ~?= "argentinaargentinaargentinaargentinaargentinaargen"
 ]

-- EJ 12
testsEjcifrarVigenere :: Test
testsEjcifrarVigenere = test [
  cifrarVigenere "computacion" "ip" ~?= "kdueciirqdv",
  cifrarVigenere "argentina campeon del mundo" "a" ~?= "argentina campeon del mundo",
  cifrarVigenere "introduccion a la programacion" "compu" ~?= "kbfgifiorcqb p no elqudpgcqudh",
  cifrarVigenere "TRAMPA" "tramposa" ~?= "TRAMPA",
  cifrarVigenere "DeMoCrAcIa" "si" ~?= "DmMwCzAkIi"
  
 ]

-- EJ 13
testsEjdescifrarVigenere :: Test
testsEjdescifrarVigenere = test [
  descifrarVigenere "kdueciirqdv" "ip" ~?= "computacion",
  descifrarVigenere "argentina campeon del mundo" "a" ~?= "argentina campeon del mundo",
  descifrarVigenere "kbfgifiorcqb p no elqudpgcqudh"  "compu" ~?= "introduccion a la programacion",
  descifrarVigenere "TRAMPA" "tramposa" ~?= "TRAMPA",
  descifrarVigenere "DmMwCzAkIi" "si" ~?= "DeMoCrAcIa"
 ]

-- EJ 14
testsEjpeorCifrado :: Test
testsEjpeorCifrado = test [
  peorCifrado "computacion" ["ip", "asdef", "ksy"] ~?= "asdef",
  peorCifrado "docente" ["a", "b", "c", "d"] ~?= "a",
  peorCifrado "alumno" ["alegria"] ~?= "alegria",
  peorCifrado "aBcDeFgH" ["bcd", "ab", "jklmnio"] ~?= "ab",
  peorCifrado "a" ["adfd", "bjkj", "cjkjk", "dajkjk", "yjkjkj", "zkjkjk"] ~?= "a"
 ]

-- EJ 15
testsEjcombinacionesVigenere :: Test
testsEjcombinacionesVigenere = test [
  combinacionesVigenere ["hola", "mundo"] ["a", "b"] "ipmb" ~?= [("hola", "b")],
  combinacionesVigenere ["argentina","brasil","peru"] ["g", "z", "a"] "gxmktzotg" ~?= [("argentina","g")],
  combinacionesVigenere["algoritmo", "adjurawso"]  ["asdg", "a" ] "adjurawso" ~?= [("algoritmo","asdg"),("adjurawso","a")],
  combinacionesVigenere ["algoritmo", "pascal"] ["b", "bbb", "bbbbb", "bbbbbbbbb"] "bmhpsjunp" ~?= [("algoritmo","b"),("algoritmo","bbb"),("algoritmo","bbbbb"),("algoritmo","bbbbbbbbb")]
 ]

-- Funciones útiles

-- margetFloat(): Float
-- asegura: res es igual a 0.00001
margenFloat = 0.00001

-- expectAny (actual: a, expected: [a]): Test
-- asegura: res es un Test Verdadero si y sólo si actual pertenece a la lista expected
expectAny :: (Foldable t, Eq a, Show a, Show (t a)) => a -> t a -> Test
expectAny actual expected = elem actual expected ~? ("expected any of: " ++ show expected ++ "\n but got: " ++ show actual)


-- expectlistProximity (actual: [Float], expected: [Float]): Test
-- asegura: res es un Test Verdadero si y sólo si:
--                  |actual| = |expected|
--                  para todo i entero tal que 0<=i<|actual|, |actual[i] - expected[i]| < margenFloat()
expectlistProximity:: [Float] -> [Float] -> Test
expectlistProximity actual expected = esParecidoLista actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esParecidoLista :: [Float] -> [Float] -> Bool
esParecidoLista actual expected = (length actual) == (length expected) && (esParecidoUnaAUno actual expected)

esParecidoUnaAUno :: [Float] -> [Float] -> Bool
esParecidoUnaAUno [] [] = True
esParecidoUnaAUno (x:xs) (y:ys) = (aproximado x y) && (esParecidoUnaAUno xs ys)

aproximado :: Float -> Float -> Bool
aproximado x y = abs (x - y) < margenFloat


-- expectAnyTuplaAprox (actual: CharxFloat, expected: [CharxFloat]): Test
-- asegura: res un Test Verdadero si y sólo si:
--                  para algun i entero tal que 0<=i<|expected|,
--                         (fst expected[i]) == (fst actual) && |(snd expected[i]) - (snd actual)| < margenFloat()

expectAnyTuplaAprox :: (Char, Float) -> [(Char, Float)] -> Test
expectAnyTuplaAprox actual expected = elemAproxTupla actual expected ~? ("expected any of: " ++ show expected ++ "\nbut got: " ++ show actual)

elemAproxTupla :: (Char, Float) -> [(Char, Float)] -> Bool
elemAproxTupla _ [] = False
elemAproxTupla (ac,af) ((bc,bf):bs) = sonAprox || (elemAproxTupla (ac,af) bs)
    where sonAprox = (ac == bc) && (aproximado af bf)



-- expectPermutacion (actual: [T], expected[T]) : Test
-- asegura: res es un Test Verdadero si y sólo si:
--            para todo elemento e de tipo T, #Apariciones(actual, e) = #Apariciones(expected, e)

expectPermutacion :: (Ord a, Show a) => [a] -> [a] -> Test
expectPermutacion actual expected = esPermutacion actual expected ~? ("expected list: " ++ show expected ++ "\nbut got: " ++ show actual)

esPermutacion :: Ord a => [a] -> [a] -> Bool
esPermutacion a b = (length a == length b) && (sort a == sort b)