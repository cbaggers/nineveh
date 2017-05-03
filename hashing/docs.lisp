(in-package :nineveh.hashing)

(define-docs
  (defun blub-blub-shub-hash
      "
-- Args --

grid-cell :vec2  -  Assumed to be an integer coordinate

-- Wikipedia Explanation --

Blum Blum Shub (BBS) is a pseudorandom number generator proposed in 1986 by
Lenore Blum, Manuel Blum and Michael Shub.  It takes the form:

    x_n_plus_1 = mod( x_n^2, M )

where M=pq is the product of two large primes p and q

-- GPU Version --

This is an implementation of the hashing function described in Marc Olano’s
MNoise Paper. It calculates pseudo-random values in the 0.0->1.0 range.

It includes an extra permutation pass to reduce the worst of the artifacts
from the classic version

It can run on 16bit and 24bit floating point hardware.

Generates a random number for each of the 4 cell corners

-- Credit --

Marc Olano - http://www.cs.umbc.edu/~olano/papers/mNoise.pdf

Brain Sharpe - For his phenominal explanations
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

")

  (defun quadratic-permutation-polynomial-hash
      "
-- Args --

grid-cell :vec2  -  Assumed to be an integer coordinate

-- Purpose --

This is an implementation of a permutation polynomial hash function.
It calculates pseudo-random values in the 0.0->1.0 range.

Generates a random number for each of the 4 cell corners

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

")

  (defun fast-32-hash
      "
-- Args --

grid-cell :vec2  -  Assumed to be an integer coordinate

-- Description --

Brian Sharpe's very fast 2D 32bit hashing function. It calculates pseudo-random
values in the 0.0->1.0 range.

Requires 32bit support.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
"))
