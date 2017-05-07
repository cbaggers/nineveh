(in-package :nineveh.hashing)

(define-docs
  (defun blum-blum-shub-hash
      "
-- Signatures --

 ((grid-cell :vec2)) --> :vec4

 ((grid-cell :vec3)) --> (values :vec4 :vec4)

grid-cell is assumed to be an integer coordinate

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

Generates a random number for each of the cell corners, each are returned as
one element of the resulting vectors.

-- Credit --

Marc Olano - http://www.cs.umbc.edu/~olano/papers/mNoise.pdf

Brain Sharpe - For his phenominal explanations
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

")

  (defun sgim-qpp-hash
      "
-- Signatures --

 ((grid-cell :vec2)) --> :vec4

 ((grid-cell :vec3)) --> (values :vec4 :vec4)

grid-cell is assumed to be an integer coordinate

-- Purpose --

This is an implementation of a quadratic permutation polynomial hash function.
It calculates pseudo-random values in the 0.0->1.0 range.

Generates a random number for each of the cell corners. Each are returned as
one element of the resulting vectors.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

Stefan Gustavson and Ian McEwan - For the permutation polynomial based gpu
hashing idea
")

  (defun sgim-qpp-hash-2-per-corner
      "
-- Signatures --

 ((grid-cell :vec2)) --> (values :vec4 :vec4)

grid-cell is assumed to be an integer coordinate

-- Purpose --

This is an implementation of a quadratic permutation polynomial hash function.
It calculates pseudo-random values in the 0.0->1.0 range.

Generates 2 random numbers for each of the cell corners. Each are returned as
one element of the resulting vectors.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

Stefan Gustavson and Ian McEwan - For the permutation polynomial based gpu
hashing idea
")

  (defun sgim-qpp-hash-3-per-corner
      "
-- Signatures --

 ((grid-cell :vec3)) --> (values :vec4 :vec4 :vec4 :vec4 :vec4 :vec4)

 ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3))
 -->
 (values :vec4 :vec4 :vec4)

grid-cell is assumed to be an integer coordinate

-- Purpose --

This is an implementation of a quadratic permutation polynomial hash function.
It calculates pseudo-random values in the 0.0->1.0 range.

This comes in 2 flavors, regular and masked.

The regular form generates 3 random numbers for each of the cell corners.
Each are returned as one element of the resulting vectors.

The masked variant generates 3 random numbers for the 4 3D cell corners. 2 of
the corners are pre-set (v0=0,0,0  v3=1,1,1) but the other two are user
definable.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/

Stefan Gustavson and Ian McEwan - For the permutation polynomial based gpu
hashing idea
")

  (defun bs-fast32-hash
      "
-- Signatures --

 ((grid-cell :vec2)) --> :vec4

 ((grid-cell :vec3)) --> (values :vec4 :vec4)

 ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3)) --> :vec4

grid-cell is assumed to be an integer coordinate

-- Description --

Brian Sharpe's fast 32bit hashing function. It calculates pseudo-random
values in the 0.0->1.0 range.

This comes in 2 flavors, regular and masked.

The regular forms generates 1 random number for each of the cell corners.
Each are returned as one element of the resulting vector/s.

The masked variant generates 1 random number for the 4 3D cell corners. 2 of
the corners are pre-set (v0=0,0,0  v3=1,1,1) but the other two are user
definable.

Requires 32bit support.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
")

  (defun bs-fast32-hash-2-per-corner
      "
-- Signatures --

 ((grid-cell :vec2)) --> (values :vec4 :vec4)

grid-cell is assumed to be an integer coordinate

-- Description --

Brian Sharpe's fast 32bit hashing function. It calculates pseudo-random
values in the 0.0->1.0 range.

This generates 2 randoms number for each of the cell corners. Each are returned
as one element of the 2 resulting vectors.

Requires 32bit support.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
")

  (defun bs-fast32-hash-3-per-corner
      "
-- Signatures --

 ((grid-cell :vec2)) --> (values :vec4 :vec4 :vec4)

 ((grid-cell :vec3)) --> (values :vec4 :vec4 :vec4 :vec4 :vec4 :vec4)

 ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3))
 -->
 (values :vec4 :vec4 :vec4)

grid-cell is assumed to be an integer coordinate

-- Description --

Brian Sharpe's fast 32bit hashing function. It calculates pseudo-random
values in the 0.0->1.0 range.

This comes in 2 flavors, regular and masked.

The regular forms generates 3 random numbers for each of the cell corners.
Each are returned as one element of the resulting vector/s.

The masked variant generates 3 random numbers for the 4 3D cell corners. 2 of
the corners are pre-set (v0=0,0,0  v3=1,1,1) but the other two are user
definable.

Requires 32bit support.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
")

  (defun bs-quick-hash
      "
-- Signatures --

 ((grid-cell :vec2)) --> :vec4

 ((grid-cell :vec3)) --> (values :vec4 :vec4)

 ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3)) --> :vec4

grid-cell is assumed to be an integer coordinate

-- Description --

Brian Sharpe's FastHash32_2 hashing function. It calculates pseudo-random
values in the 0.0->1.0 range.

An alternative to bs-fast32-hash that is:
- slightly slower
- can have a larger domain
- allows for a 4D implementation

This generates 1 random number for each of the cell corners. Each are returned
as one element of the resulting vector/s.

Requires 32bit support.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
")

  (defun bs-quick-hash-4-per-corner
      "
-- Signatures --

 ((grid-cell :vec2)) --> :vec4

 ((grid-cell :vec3)) --> (values :vec4 :vec4)

 ((grid-cell :vec3) (v1-mask :vec3) (v2-mask :vec3)) --> :vec4

grid-cell is assumed to be an integer coordinate

-- Description --

Brian Sharpe's FastHash32_2 hashing function. It calculates pseudo-random
values in the 0.0->1.0 range.

An alternative to bs-fast32-hash that is:
- slightly slower
- can have a larger domain
- allows for a 4D implementation

This generates 4 random numbers for each of the cell corners. Each are returned
as one element of the resulting vector/s.

Requires 32bit support.

-- Credit --

Brain Sharpe - The implementation is taken from his excellent article here:
https://briansharpe.wordpress.com/2011/11/15/a-fast-and-simple-32bit-floating-point-hash-function/
"))
