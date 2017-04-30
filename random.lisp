(in-package :nineveh)

(defun-g rand ((co :vec2))
  (fract (* (sin (dot (s~ co :xy) (glsl-expr "vec2(12.9898,78.233)" :vec2)))
            (glsl-expr "43758.5453" :float))))
