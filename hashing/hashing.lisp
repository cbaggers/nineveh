(in-package :nineveh.hashing)

(defun-g blum-blum-shub-hash ((grid-cell :vec2))
  (let* (;; coord-prepare
         (v4 (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1f0)) ))
         (hash-coord (mod-fixed-denominator v4 61f0))
         ;; permute 0
         (v4 (s~ hash-coord :xzxz))
         (hash (mod-fixed-denominator-low-quality (* v4 v4) 61f0))
         ;; permute 1 - Not in original paper but lessens the worst artifacts
         (v4 (+ hash (s~ hash-coord :yyww)))
         (hash (mod-fixed-denominator-low-quality (* v4 v4) 61f0))
         ;; permute-and-resolve
         (v4 (+ hash (s~ hash-coord :xzxz)))
         (result (fract (* v4 v4 (/ 1f0 61f0)))))
    result))
