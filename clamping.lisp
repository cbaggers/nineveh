(in-package #:nineveh)

(defun-g saturate ((val :float))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :vec2))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :vec3))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :vec4))
  (clamp val 0s0 1s0))
