(in-package :nineveh.math-primitives)

(defun-g atan2 ((x :float) (y :float))
  (atan y x))

(v-define-compiler-macro atan2 ((x :float) (y :float))
  `(atan ,y ,x))

(defun-g atan2 ((x :vec2) (y :vec2))
  (atan y x))

(v-define-compiler-macro atan2 ((x :vec2) (y :vec2))
  `(atan ,y ,x))

(defun-g atan2 ((x :vec3) (y :vec3))
  (atan y x))

(v-define-compiler-macro atan2 ((x :vec3) (y :vec3))
  `(atan ,y ,x))

(defun-g atan2 ((x :vec4) (y :vec4))
  (atan y x))

(v-define-compiler-macro atan2 ((x :vec4) (y :vec4))
  `(atan ,y ,x))
