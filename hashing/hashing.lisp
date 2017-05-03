(in-package :nineveh.hashing)

(defun-g blum-blum-shub-hash ((grid-cell :vec2))
  (let* (;; bbs-coord-prepare
         (v4 (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1f0)) ))
         (hash-coord (mod-fixed-denominator v4 61f0))
         ;; bbs-permute
         (v4 (s~ hash-coord :xzxz))
         (hash (mod-fixed-denominator-low-quality (* v4 v4) 61f0))
         ;; bbs-permute-and-resolve
         (v4 (+ hash (s~ hash-coord :yyww)))
         (result (fract (* v4 v4 (/ 1f0 61f0)))))
    result))
