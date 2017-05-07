(in-package :nineveh.hashing)

;;  3x^2-2x^3  ( Hermine Curve.  Same as SmoothStep().  As used by Perlin in Original Noise. )

(defun-g perlin-hermine ((x :float))
  (* x (* x (- 3.0 (* 2.0 x)))))

(defun-g perlin-hermine ((x :vec2))
  (* x (* x (- (v2! 3.0) (* 2.0 x)))))

(defun-g perlin-hermine ((x :vec3))
  (* x (* x (- (v3! 3.0) (* 2.0 x)))))

(defun-g perlin-hermine ((x :vec4))
  (* x (* x (- (v4! 3.0) (* 2.0 x)))))


;; 6x^5-15x^4+10x^3 Quintic Curve.
;; As used by Perlin in Improved Noise: http://mrl.nyu.edu/~perlin/paper445.pdf

(defun-g perlin-quintic ((x :float))
  (* x (* x (* x (+ (* x (- (* x 6.0) 15.0)) 10.0)))))

(defun-g perlin-quintic ((x :vec2))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v2! 15.0)))
                    (v2! 10.0))))))

(defun-g perlin-quintic ((x :vec3))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v3! 15.0)))
                    (v3! 10.0))))))

(defun-g perlin-quintic ((x :vec4))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v4! 15.0)))
                    (v4! 10.0))))))

(defun-g perlin-quintic-interp-and-deriv ((x :vec2))
  (* (s~ x :xyxy)
     (* (s~ x :xyxy)
        (+
         (* (s~ x :xyxy)
            (+
             (* (s~ x :xyxy)
                (+ (* (s~ x :xyxy) (s~ (v2! 6.0 0.0) :xxyy))
                   (s~ (v2! (- 15.0) 30.0) :xxyy)))
             (s~ (v2! 10.0 (- 60.0)) :xxyy)))
         (s~ (v2! 0.0 30.0) :xxyy)))))

(defun-g perlin-quintic-deriv ((x :vec3))
  (* x (* x (+ (* x (- (* x 30.0) (v3! 60.0)))
               (v3! 30.0)))))

;; 7x^3-7x^4+x^7
;; Faster than Perlin Quintic.  Not quite as good shape.

(defun-g perlin-quintic-fast ((x :float))
  (let* ((x3 (* x (* x x))))
    (* (+ 7.0 (* (- x3 7.0) x)) x3)))

(defun-g perlin-quintic-fast ((x :vec2))
  (let* ((x3 (* x (* x x))))
    (* (+ (v2! 7.0) (* (- x3 (v2! 7.0)) x)) x3)))

(defun-g perlin-quintic-fast ((x :vec3))
  (let* ((x3 (* x (* x x))))
    (* (+ (v3! 7.0) (* (- x3 (v3! 7.0)) x)) x3)))

(defun-g perlin-quintic-fast ((x :vec4))
  (let* ((x3 (* x (* x x))))
    (* (+ (v4! 7.0) (* (- x3 (v4! 7.0)) x)) x3)))
