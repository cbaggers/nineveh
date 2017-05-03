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

(defun-g quadratic-permutation-polynomial-hash ((grid-cell :vec2))
  ;; grid-cell is assumed to be an integer coordinate
  (labels ((sgpp-permute ((x :vec4))
             (* (fract (* x (+ (* (/ 34f0 289f0) x) (v4! (/ 1f0 289f0)))))
                289f0))
           (sgpp-resolve ((x :vec4))
             (let ((k (/ 7f0 288f0)))
               (fract (* x k)))))
    (let* (;; coord-prepare
           (v4 (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1f0))))
           (hash-coord (mod-fixed-denominator v4 289f0))
           ;; permute 0
           (hash (sgpp-permute (s~ hash-coord :xzxz)))
           ;; permute 1
           (hash (sgpp-permute (+ hash (s~ hash-coord :yyww))))
           ;; resolve
           (result (sgpp-resolve hash)))
      result)))

(defun-g fast-32-hash ((grid-cell :vec2))
  (let* ((offset (v! 26f0 161f0))
         (domain 71f0)
         ;; I use glsl-expr here as I dont' want particular CL implementation
         ;; differences to affect the number.
         (some-large-float (glsl-expr "951.135664" :float))
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1f0))))
         ;; truncate the domain
         (p (- p (* (floor (* p (/ 1f0 domain))) domain)))
         ;; offset to interesting part of the noise
         (p (+ p (s~ offset :xyxy)))
         ;; calculate and return the hash
         (p (* p p))
         (result (fract (* (v4! (/ 1f0 some-large-float))
                           (s~ p :xzxz)
                           (s~ p :yyww)))))
    result))
