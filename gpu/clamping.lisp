(in-package #:nineveh)

(defun-g saturate ((val :float))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :vec2))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :vec3))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :vec4))
  (clamp val 0s0 1s0))

(defun-g saturate ((val :double))
  (clamp val 0d0 1d0))

(defun-g saturate ((val :dvec2))
  (clamp val 0d0 1d0))

(defun-g saturate ((val :dvec3))
  (clamp val 0d0 1d0))

(defun-g saturate ((val :dvec4))
  (clamp val 0d0 1d0))
