import Test.HUnit
import Solucion
import Data.List

-- Ej 1
testEsMinuscula :: Test
testEsMinuscula = test [
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

-- Ej 2
testLetraANatural :: Test
testLetraANatural = test [
  "letraANatural 'a'" ~: letraANatural 'a' ~=? 0,
  "letraANatural 'b'" ~: letraANatural 'b' ~=? 1,
  "letraANatural 'c'" ~: letraANatural 'c' ~=? 2,
  "letraANatural 'd'" ~: letraANatural 'd' ~=? 3,
  "letraANatural 'e'" ~: letraANatural 'e' ~=? 4,
  "letraANatural 'f'" ~: letraANatural 'f' ~=? 5,
  "letraANatural 'w'" ~: letraANatural 'w' ~=? 22,
  "letraANatural 'x'" ~: letraANatural 'x' ~=? 23,
  "letraANatural 'y'" ~: letraANatural 'y' ~=? 24,
  "letraANatural 'z'" ~: letraANatural 'z' ~=? 25,
  "letraANatural 'A'" ~: letraANatural 'A' ~=? -1
 ]

 -- Ej 3
testDesplazar :: Test
testDesplazar = test [
  "testDesplazar 'a' 3" ~: desplazar 'a' 3 ~=? 'd',
  "testDesplazar 'b' 2" ~: desplazar 'b' 2 ~=? 'd',
  "testDesplazar 'a' 26" ~: desplazar 'a' 26 ~=? 'a',
  "testDesplazar 'a' 27" ~: desplazar 'a' 27 ~=? 'b',
  "testDesplazar 'a' -1" ~: desplazar 'a' (-1) ~=? 'b',
  "testDesplazar 'a' -2" ~: desplazar 'a' (-2) ~=? 'c',
  "testDesplazar 'c' -1" ~: desplazar 'c' (-1) ~=? 'b',
  "testDesplazar 'd' -2" ~: desplazar 'd' (-2) ~=? 'c',
  "testDesplazar 'a' 1000" ~: desplazar 'a' 1000 ~=? 'm',
  "testDesplazar 'a' -1000" ~: desplazar 'a' (-1000) ~=? 'm',
  "testDesplazar 'e' 5" ~: desplazar 'e' 5 ~=? 'j',
  "testDesplazar 'f'" ~: desplazar 'f' 1 ~=? 'g',
  "testDesplazar 'w'" ~: desplazar 'A' 1 ~=? 'A',
  "testDesplazar 'x'" ~: desplazar 'B' 1 ~=? 'B',
  "testDesplazar 'y'" ~: desplazar 'C' 1 ~=? 'C',
  "testDesplazar 'z'" ~: desplazar 'D' 1 ~=? 'D',
  "testDesplazar 'A'" ~: desplazar 'E' 1 ~=? 'E'
 ]

allTests :: Test
allTests = test [
    "esMinuscula" ~: testEsMinuscula,
    "testLetraANatural" ~: testLetraANatural,
    "testDesplazar" ~: testDesplazar
 ]

runAllTests :: IO Counts
runAllTests = runTestTT allTests