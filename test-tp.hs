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

allTests :: Test
allTests = test [
    "esMinuscula" ~: testEsMinuscula,
    "testLetraANatural" ~: testLetraANatural
 ]

runAllTests :: IO Counts
runAllTests = runTestTT allTests