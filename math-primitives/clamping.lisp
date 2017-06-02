(in-package #:nineveh.math-primitives)

(defun-g-equiv saturate ((val :float))
  (clamp val 0s0 1s0))

(defun-g-equiv saturate ((val :vec2))
  (clamp val 0s0 1s0))

(defun-g-equiv saturate ((val :vec3))
  (clamp val 0s0 1s0))

(defun-g-equiv saturate ((val :vec4))
  (clamp val 0s0 1s0))

(defun-g-equiv saturate ((val :double) &context :410 :420 :430 :440 :450)
  (clamp val 0d0 1d0))

(defun-g-equiv saturate ((val :dvec2) &context :410 :420 :430 :440 :450)
  (clamp val 0d0 1d0))

(defun-g-equiv saturate ((val :dvec3) &context :410 :420 :430 :440 :450)
  (clamp val 0d0 1d0))

(defun-g-equiv saturate ((val :dvec4) &context :410 :420 :430 :440 :450)
  (clamp val 0d0 1d0))
