Gamma
=========

[![Build Status](https://travis-ci.org/rockbmb/gamma2.svg?branch=master)](https://travis-ci.org/rockbmb/gamma2)

`gamma` is a number theory package written in Haskell that aims to provide its users
with the following functionality:

* a type`class Num a => Factorial a` that provides a `factorial` function
* a type`class (Eq a, Floating a, Factorial a) => Gamma a` that provides the functions
  + `gamma`
  + `lnGamma` (natural logarithm of the Gamma function) and
  + `lnFactorial` (natural logarithm of the factorial function)
* a type`class Gamma a => IncGamma a` for the [incomplete lower and upper Gamma functions](https://en.wikipedia.org/wiki/Incomplete_gamma_function).
* a type`class Gamma a => GenGamma a` to represent the [multivariate gamma function](https://en.wikipedia.org/wiki/Multivariate_gamma_function)