(in-package :nineveh.math-primitives)

(define-docs
  (defmacro mod-fixed-denominator
      "
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
Like the regular mod-fixed-denominator macro, this macro emits an
implementation of mod with a fixed denominator in a form that shader compilers
can trivially optimize away the divide..HOWEVER it is also faster and lower
quality (it suffers from precision provlems).

It is very likely that your implmentation performs with optimization for mod
anyway. However some may recommend doing it anyway.

-- Credit --

Brian Sharpe - https://briansharpe.wordpress.com/2011/10/01/gpu-texture-free-noise/"))
