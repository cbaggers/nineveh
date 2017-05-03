(in-package :nineveh.hashing)

;;------------------------------------------------------------

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

;;------------------------------------------------------------

(defun-g sgpp-coord-prepare ((x :vec4))
  (mod-fixed-denominator x 289f0))

(defun-g sgpp-coord-prepare ((x :vec3))
  (mod-fixed-denominator x 289f0))

(defun-g sgpp-permute ((x :vec4))
  (* (fract (* x (+ (* (/ 34f0 289f0) x) (v4! (/ 1f0 289f0)))))
     289f0))

(defun-g sgpp-resolve ((x :vec4))
  (let ((k (/ 7f0 288f0)))
    (fract (* x k))))

;;-----------
;; 2D

(defun-g quadratic-permutation-polynomial-hash ((grid-cell :vec2))
  ;; generates a random number for each of the 4 cell corners
  ;; grid-cell is assumed to be an integer coordinate
  (let* (;; coord-prepare
         (v4 (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1f0))))
         (hash-coord (sgpp-coord-prepare v4))
         ;; permute 0
         (hash (sgpp-permute (s~ hash-coord :xzxz)))
         ;; permute 1
         (hash (sgpp-permute (+ hash (s~ hash-coord :yyww)))))
    ;; resolve
    (sgpp-resolve hash)))

(defun-g quadratic-permutation-polynomial-hash-2-val ((grid-cell :vec2))
  ;; generates 2 random numbers for each of the 4 cell corners
  ;; grid-cell is assumed to be an integer coordinate
  (let* (;; coord-prepare
         (v4 (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1f0))))
         (hash-coord (sgpp-coord-prepare v4))
         ;; permute 0
         (hash (sgpp-permute (s~ hash-coord :xzxz)))
         ;; permute 1
         (hash (sgpp-permute (+ hash (s~ hash-coord :yyww)))))
    ;; resolve
    (values (sgpp-resolve (sgpp-permute hash))
            (sgpp-resolve hash))))

;;-----------
;; 3D

(defun-g quadratic-permutation-polynomial-hash ((grid-cell :vec3))
  ;; generates a random number for each of the 8 cell corners
  ;; grid-cell is assumed to be an integer coordinate
  (let* (;; coord-prepare
         (grid-cell (sgpp-coord-prepare grid-cell))
         (grid-cell-inc1 (* (step grid-cell (v3! 287.5f0))
                            (+ grid-cell (v3! 1f0))))
         ;; highz-hash
         (hash (sgpp-permute
                (+ (sgpp-permute
                    (s~ (v! (x grid-cell) (x grid-cell-inc1)) :xyxy))
                   (s~ (v! (y grid-cell) (y grid-cell-inc1)) :xxyy)))))
    (values
     ;; highz-hash
     (sgpp-resolve (sgpp-permute (+ hash (s~ grid-cell :zzzz))))
     ;; lowz-hash
     (sgpp-resolve (sgpp-permute (+ hash (s~ grid-cell-inc1 :zzzz)))))))

;;------------------------------------------------------------

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

;;------------------------------------------------------------
