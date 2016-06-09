# neural - Neural Nets in native Haskell

[![Build Status](https://travis-ci.org/brunjlar/neural.svg?branch=master)](https://travis-ci.org/brunjlar/neural)

## Motivation

The goal of this project is to provide a flexible framework for 
[neural networks](https://en.wikipedia.org/wiki/Artificial_neural_network) 
(and similar parameterized models) in Haskell.

There are already a couple of neural network libraries out there on Hackage, but as far as I can tell,
they either

- are wrappers for an engine written in another language or
- offer a limitted choice of network architectures, training algorithms or error functions
  or are not easily extensible.

The goal of this library is to have an implementation in native Haskell (reasonably efficient)
which offers maximal flexibility.

Furthermore, [gradient descent/backpropagation](https://en.wikipedia.org/wiki/Backpropagation) should work automatically, using
[automatic differentiation](https://hackage.haskell.org/package/ad-4.3.2/docs/Numeric-AD.html).
This means that new and complicated activation functions and/or network architectures can be used without the need
to first calculate derivatives by hand.

In order to provide a powerful and flexible API, models are constructed using *components* which implement the
[Arrow and ArrowChoice](https://hackage.haskell.org/package/base-4.9.0.0/docs/Control-Arrow.html) typeclasses. 
They can therefore easily be combined and transformed, using a multitude of
available combinators or [arrow notation](http://downloads.haskell.org/~ghc/8.0.1/docs/html/users_guide/glasgow_exts.html#arrow-notation).

Even though neural networks are the primary motivation for this project, any other kind of model can be
defined in the same framework, whenever the model depends on a collection of numerical parameters in a differentiable
way. - One simple example for this would be [linear regression](https://en.wikipedia.org/wiki/Linear_regression).

## Examples

At the moment, two examples are included:

- [sqrt](examples/sqrt) models the regression problem of approximating the square root function on the interval [0,4].

- [iris](examples/iris) solves the famous [Iris Flower](https://en.wikipedia.org/wiki/Iris_flower_data_set) classification problem.
