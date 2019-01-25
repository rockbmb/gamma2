#!/usr/bin/env runhaskell
module Main where

import qualified Math.GammaTests (tests)
import qualified Math.IncGammaTests (tests)

import Test.Framework (defaultMain, testGroup)

main = defaultMain 
    [ testGroup "Math.Gamma"            Math.GammaTests.tests
    , testGroup "Math.Gamma.Incomplete" Math.IncGammaTests.tests
    ]