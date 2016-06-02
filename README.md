# neural - Neural Nets in native Haskell

The goal of this project is the provide a flexible framework for neural networks (and similar models)
in Haskell.

There are already a couple of neural network libraries out there on Hackage, but as far as I can tell,
they either

- are wrappers for an engine written in another language or
- offer a limitted choice of network architectures, training algorithms or error functions
  and are not easily extensible.

The goal of this library is to have an implementation in native Haskell (and yet is reasonably efficient)
and offers maximal flexibility.
