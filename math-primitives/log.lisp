(in-package #:nineveh.math-primitives)

(defconstant +k-log-base-10+ (/ 1.0 (log 10.0 2)))

(defun-g log10 ((n :float))
  (* (log2 n) +k-log-base-10+))

(defun-g log10 ((n :vec2))
  (v! (* (log2 (x n)) +k-log-base-10+)
      (* (log2 (y n)) +k-log-base-10+)))

(defun-g log10 ((n :vec3))
  (v! (* (log2 (x n)) +k-log-base-10+)
      (* (log2 (y n)) +k-log-base-10+)
      (* (log2 (z n)) +k-log-base-10+)))

(defun-g log10 ((n :vec4))
  (v! (* (log2 (x n)) +k-log-base-10+)
      (* (log2 (y n)) +k-log-base-10+)
      (* (log2 (z n)) +k-log-base-10+)
      (* (log2 (w n)) +k-log-base-10+)))
