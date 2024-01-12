module Spec where

import Tarefa2_202324li1g005_Spec
import Tarefa1_202324li1g005_Spec
import Test.HUnit

test_suite_01 = test ["Basic Test" ~: True ~=? True]

main :: IO ()
main = runTestTTAndExit $ test [test_suite_01, testesTarefa1, testesTarefa2]
