# neural - Neural Nets in native Haskell

The goal of this project is to provide a flexible framework for neural networks 
(and similar parameterized models) in Haskell.

There are already a couple of neural network libraries out there on Hackage, but as far as I can tell,
they either

- are wrappers for an engine written in another language or
- offer a limitted choice of network architectures, training algorithms or error functions
  or are not easily extensible.

The goal of this library is to have an implementation in native Haskell (reasonably efficient)
which offers maximal flexibility.

Furthermore, *gradient descent* (*backpropagation*) should work automatically, using *automatic differentiation*.
This means that new and complicated activation functions and/or network architectures can be used without the need
to first calculate derivatives by hand.

In order to provide a powerful and flexible API, models are constructed using *components* which implement the
*Arrow* and *ArrowChoice* typeclasses. They can therefore easily be combined and transformed, using a multitude of
available combinators or *arrow notation*.

Even though neural networks are the primary motivation for this project, any other kind of model can be
defined in the same framework, whenever the model depends on a collection of numerical parameters in a differentiable
way. - One simple example for this would be *linear regression*.
