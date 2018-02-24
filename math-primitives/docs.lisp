(in-package :nineveh.math-primitives)

(docs:define-docs
  (defmacro mod-fixed-denominator
      "
-- Args --

val - a form (should evaluate to a :float)

denominator - a constant float or integer

-- Purpose --

This macro emits an implementation of mod with a fixed denominator in a form
that shader compilers can trivially optimize away the divide.

It is very likely that your implmentation performs with optimization for mod
anyway. However some may recommend doing it anyway.

-- Credit --

Marc Olano - http://www.cs.umbc.edu/%7Eolano/papers/index.html#mNoise

Brian Sharpe - For his excellent explanations here ↓
https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/")

  (defmacro mod-fixed-denominator-low-quality
      "
-- Args --

val - a form (should evaluate to a :float)

denominator - a constant float or integer

-- Purpose --

Like the regular mod-fixed-denominator macro, this macro emits an
implementation of mod with a fixed denominator in a form that shader compilers
can trivially optimize away the divide..HOWEVER it is also faster and lower
quality (it suffers from precision provlems).

It is very likely that your implmentation performs with optimization for mod
anyway. However some may recommend doing it anyway.

-- Credit --

Brian Sharpe - https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/"))
