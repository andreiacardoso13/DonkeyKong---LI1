module Main where

import Tarefa1_202324li1g005_Spec
import Tarefa2_202324li1g005_Spec
import Tarefa3_202324li1g005_Spec
import Tarefa4_202324li1g005_Spec
import Test.HUnit

test_suite_01 = test ["Basic Test" ~: True ~=? True]

main :: IO ()
main = runTestTTAndExit $ test [test_suite_01, testesTarefa1, testesTarefa2, testesTarefa3, testesTarefa4]
