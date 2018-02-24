(in-package :nineveh.random)

(docs:define-docs
  (defun rand
      "
-- Arg --

seed :vec2

-- Purpose --

Returns a 'random' float.

-- Explaination --

Based on the fact that sin(<huge multiplier>*x)
modulates extremely quickly. So quickly that sampling the sin
function at every fragment location effectively gives you “random”
numbers

-- Notes --

This could have issues on some ES/WebGL implementations. Some implementations
might not be preconditioning sin to a reasonable 2PI range. This has been shown
to cause issues before.

At the time of writing Varjo does not support float precision declarations but
when it does we can provide a safer implementation of this

-- Credit --

Impementation - Unknown but see this possible answer:
                http://stackoverflow.com/a/34223787/574033

Notes - http://byteblacksmith.com/improvements-to-the-canonical-one-liner-glsl-rand-for-opengl-es-2-0/"))
