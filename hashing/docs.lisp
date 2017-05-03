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

-- Credit --

Marc Olano - http://www.cs.umbc.edu/~olano/papers/mNoise.pdf

Brain Sharpe - For his phenominal explanations
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

"))
