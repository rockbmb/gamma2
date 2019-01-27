{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE ImplicitParams   #-}
{-# OPTIONS_GHC -fno-warn-type-defaults #-}

module Math.GammaTests
    ( eps
    , isSane
    , tests
    , (~=)
    ) where

import qualified Math.Gamma                 as G
import Math.Reference                       (err, lgamma, tgamma)

import Data.Complex                         (Complex (..), conjugate, imagPart,
                                             magnitude, realPart)
import Test.Framework                       (Test, testGroup)
import Test.Framework.Providers.QuickCheck2 (testProperty)
import Test.QuickCheck                      (Arbitrary, NonNegative (..), Positive (..),
                                             (==>))

gammaSign :: (RealFrac a, Fractional t) => a -> t
gammaSign x
    | x > 0     = 1
    | otherwise = case properFraction x of
        (_, 0)              -> 0/0
        (n, _)  | odd n     ->   1
                | otherwise ->  -1

gammaArg :: RealFrac a => a -> Bool
gammaArg = not . isNaN . gammaSign

eps :: RealFloat a => a
eps = eps'
    where
        eps' = encodeFloat 1 (1 - floatDigits eps')

infix 4 ~=
(~=) :: (Ord a, Num a, ?eps::a, ?mag::t -> a, Num t)
     => t -> t -> Bool
x ~= y
    =  absErr <= ?eps
    || absErr <= ?eps * min (?mag x) (?mag y)
    where absErr = ?mag (x-y)

isSane :: RealFloat a => a -> Bool
isSane x = all (\f -> not (f x)) [isNaN, isInfinite, isDenormalized]

tests :: [Test]
tests =
    [ testGroup "Float"
        [ testGroup "Native"  (realTests G.gamma (G.lnGamma :: Float -> Float))
        , testGroup "Complex" (complexTests G.gamma (G.lnGamma :: Complex Float -> Complex Float))
        ]
    , testGroup "Double"
        [ testGroup "Native"  (realTests    G.gamma (G.lnGamma :: Double -> Double))
        , testGroup "FFI"     (realTests    tgamma lgamma)
        , testGroup "Complex" (complexTests G.gamma (G.lnGamma :: Complex Double -> Complex Double))
        ]
    ]

realTests :: (Arbitrary a, G.Gamma a, Show a, RealFloat a)
          => (a -> a) -> (a -> a) -> [Test]
realTests gamma lnGamma = 
    let ?mag = abs
        ?eps = eps
        ?complex = False
     in [ testGroup "gamma"       (realGammaTests    gamma)
        , testGroup "lnGamma"     (realLogGammaTests gamma lnGamma)
        , testGroup "lnFactorial" (logFactorialTests lnGamma G.lnFactorial)
        ]

complexTests :: (Arbitrary a, G.Gamma (Complex a), G.Gamma a, Show a, RealFloat a)
             => (Complex a -> Complex a) -> (Complex a -> Complex a) -> [Test]
complexTests gamma lnGamma = 
    let ?mag = magnitude
        ?eps = eps
        ?complex = True
     in [ testGroup "gamma"   (complexGammaTests    gamma)
        , testGroup "lnGamma" (complexLogGammaTests gamma lnGamma)
        , testGroup "lnFactorial" (logFactorialTests lnGamma G.lnFactorial)
        ]

realGammaTests
    :: (Arbitrary a, ?mag::a -> t, ?complex::Bool, Show a, RealFloat a, RealFloat t)
    => (a -> a) -> [Test]
realGammaTests gamma = 
    gammaTests gamma ++
    [ testProperty "between factorials" $ \(Positive x) -> 
        let gam w = fromInteger (product [1..w-1])
            gamma_x = gamma x `asTypeOf` eps
         in x > 2 && isSane gamma_x
            ==> gam (floor x) <= gamma_x && gamma_x <= gam (ceiling x)
    , testProperty "agrees with factorial" $ \(Positive x) ->
        let gam w = fromInteger (product [1..w-1])
            gamma_x = gamma (fromInteger x)
         in isSane gamma_x ==> 
            let ?eps = 16*eps in gam x ~= gamma_x
    , testProperty "agrees with C tgamma" $ \x ->
        let a = gamma x
            b = realToFrac (tgamma (realToFrac x))
         in isSane a ==> 
            let ?eps = 512*eps in a ~= b
    , testProperty "agrees with reference implementation" $ \x ->
        let a = gamma x
         in isSane a ==> snd (err gamma x) <= 256*eps
    , testProperty "monotone when x>2" $ \(Positive x) ->
        let x' = x * (1 + 256*eps)
            a = gamma x
            b = gamma x'
         in (x > 2) && (x <= x') && all isSane [a,b] ==> a <= b
    , testProperty "domain check" $ \x ->
        let a = gamma x
         in not (isInfinite a) ==>
            isNaN a == not (gammaArg x)
    , testProperty "sign check" $ \x ->
        let a = gamma x; s = gammaSign x
            signum' z@0 | isNegativeZero z  = -1
                        | otherwise         = 1
            signum' z = signum z
         in gammaArg x && not (isInfinite a) ==>
            signum' a == s
    ]


complexGammaTests
    :: (Arbitrary a1, G.Gamma a1, ?mag::Complex a1 -> a, ?eps::a,
        ?complex::Bool, Show a1, RealFloat a1, RealFloat a)
    => (Complex a1 -> Complex a1) -> [Test]
complexGammaTests gamma = 
    gammaTests gamma ++
    [ testProperty "conjugate" $ \x -> 
        let gam = gamma x
         in isSane (magnitude gam) ==> conjugate gam ~= gamma (conjugate x)
    , testProperty "real argument" $ \(Positive x) ->
        let z = x :+ 0
            gam = G.gamma x
         in isSane gam ==> 
            let ?mag = abs; ?eps = 512 * eps
             in gam ~= realPart (gamma z)
    ]

gammaTests
    :: (Arbitrary t2, ?mag::t2 -> a, ?complex::Bool, Show t2,
       RealFloat a, Floating t2)
    => (t2 -> t2) -> [Test]
gammaTests gamma =
    [ testProperty "increment arg" $ \x ->
        let a = gamma x
            b = gamma (x + 1)
            margin  | ?complex  = 32
                    | otherwise = 32
         in all (isSane . ?mag) [a,b,recip a, recip b]
            ==> ?mag (a - b/x) <= margin * (max 2 (1 + recip (?mag x))) * eps * ?mag a
             || ?mag (a*x - b) <= margin * (max 2 (1 +        ?mag x))  * eps * ?mag b
    , testProperty "reflect" $ \x ->
        ?mag x > 0 ==>
        let a = gamma x
            b = gamma (1 - x)
            c = sin (pi * x) / pi
            margin  | ?complex  = 16
                    | otherwise = 256
         in all (isSane . ?mag) [a,b,c]
            -- There may be tighter bounds to be found but I haven't
            -- been able to derive them yet.
            ==> ?mag (a*b*c-1) <= margin * eps * (1 + ?mag c * (1 + ?mag (a+b)))
             || ?mag (a*b*c-1) <= margin * eps * (?mag (a*b) + ?mag (a*c) + ?mag (b*c))
    ]

logGammaTests
    :: (Arbitrary t, ?mag::t -> t1, ?complex::Bool, Show t,
        RealFloat t1, Ord a1, Ord a, Num a1, Num a, Floating t)
    => (t -> t) -> (t -> t) -> (t -> a) -> (t -> a1) -> [Test]
logGammaTests gamma lnGamma real imag =
    [ testProperty "increment arg" $ \x ->
        let gam = lnGamma (x+1)
         in real x > 0 && isSane (?mag gam)
            ==> 
            let ?eps = 32 * eps
             in gam - log x ~= lnGamma x
                || gam ~= log x + lnGamma x
    , testProperty "reflect" $ \x ->
        ?mag x > 0 ==>
        let a = lnGamma x
            b = lnGamma (1 - x)
            c = log pi - c';    c' = log (sin (pi * x))
         in all (isSane . ?mag) [a,b,c] 
            ==> 
            let ?eps = 512 * eps
             in     a + b ~= c
                 || a ~= c - b
                 || b ~= c - a
                 || a - b - c' ~= log pi
    , testProperty "agrees with log . gamma" $ \x ->
        let a = log b'    ; a' = exp b
            b = lnGamma x ; b' = gamma x
            margin  | ?complex   = 1024
                    | otherwise  = 16
         in   (real x > 1 || abs (imag x) > 1)
            && all (isSane . ?mag) [a,b,a',b'] ==>
            let ?eps = margin * eps
             in a ~= b || a' ~= b'
    ]

realLogGammaTests
    :: (Arbitrary a, Show a, RealFloat a)
    => (a -> a) -> (a -> a) -> [Test]
realLogGammaTests gamma lnGamma = 
    let ?mag = abs
        ?complex = False
     in logGammaTests gamma lnGamma id (const 0) ++
        [ testProperty "between factorials" $ \(Positive x) -> 
            let gam w = sum $ map (log.fromInteger) [1 .. w-1]
                gamma_x = lnGamma x `asTypeOf` eps
             in x > 2 && isSane gamma_x
                ==> gam (floor x) <= gamma_x && gamma_x <= gam (ceiling x)
        , let ?eps = 2 * eps
           in testProperty "agrees with C lgamma" $ \(NonNegative x) ->
            let a = lnGamma x
                b = realToFrac (lgamma (realToFrac x))
             in let ?eps = 512 * eps
                 in isSane a ==> a ~= b
        ]

complexLogGammaTests
    :: (Arbitrary a, G.Gamma a, Show a, RealFloat a)
    => (Complex a -> Complex a) -> (Complex a -> Complex a) -> [Test]
complexLogGammaTests gamma lnGamma = 
    let ?mag = magnitude
        ?complex = True
     in logGammaTests gamma lnGamma realPart imagPart ++
        [ testProperty "real argument" $ \(Positive x) ->
            let z = x :+ 0
                gam = G.lnGamma x
             in isSane gam ==> 
                let ?eps = 8 * eps
                 in (gam :+ 0) ~= lnGamma z
        ]

logFactorialTests
    :: (?mag::t -> a, ?eps::a, RealFloat a, Num t1, Num t)
    => (t1 -> t) -> (Integer -> t) -> [Test]
logFactorialTests lnGamma lnFactorial =
    [ testProperty "agrees with lnGamma" $ \x ->
        let gam = lnGamma (fromInteger x + 1)
         in isSane (?mag gam)
            ==> gam ~= lnFactorial x
    ]
