(in-package :nineveh.shaping-functions)

;;  3x^2-2x^3  ( Hermine Curve.  Same as SmoothStep().  As used by Perlin in Original Noise. )

(defun-g hermine ((x :float))
  (* x (* x (- 3.0 (* 2.0 x)))))

(defun-g hermine ((x :vec2))
  (* x (* x (- (v2! 3.0) (* 2.0 x)))))

(defun-g hermine ((x :vec3))
  (* x (* x (- (v3! 3.0) (* 2.0 x)))))

(defun-g hermine ((x :vec4))
  (* x (* x (- (v4! 3.0) (* 2.0 x)))))


;; 6x^5-15x^4+10x^3 Quintic Curve.
;; As used by Perlin in Improved Noise: http://mrl.nyu.edu/~perlin/paper445.pdf

(defun-g quintic ((x :float))
  (* x (* x (* x (+ (* x (- (* x 6.0) 15.0)) 10.0)))))

(defun-g quintic ((x :vec2))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v2! 15.0)))
                    (v2! 10.0))))))

(defun-g quintic ((x :vec3))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v3! 15.0)))
                    (v3! 10.0))))))

(defun-g quintic ((x :vec4))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v4! 15.0)))
                    (v4! 10.0))))))

(defun-g quintic-interp-and-deriv ((x :vec2))
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

(defun-g quintic-deriv ((x :vec3))
  (* x (* x (+ (* x (- (* x 30.0) (v3! 60.0)))
               (v3! 30.0)))))

;; 7x^3-7x^4+x^7
;; Faster than Perlin Quintic.  Not quite as good shape.

(defun-g quintic-fast ((x :float))
  (let* ((x3 (* x (* x x))))
    (* (+ 7.0 (* (- x3 7.0) x)) x3)))

(defun-g quintic-fast ((x :vec2))
  (let* ((x3 (* x (* x x))))
    (* (+ (v2! 7.0) (* (- x3 (v2! 7.0)) x)) x3)))

(defun-g quintic-fast ((x :vec3))
  (let* ((x3 (* x (* x x))))
    (* (+ (v3! 7.0) (* (- x3 (v3! 7.0)) x)) x3)))

(defun-g quintic-fast ((x :vec4))
  (let* ((x3 (* x (* x x))))
    (* (+ (v4! 7.0) (* (- x3 (v4! 7.0)) x)) x3)))


;; Quintic Hermite Interpolation
;; http://www.rose-hulman.edu/~finn/CCLI/Notes/day09.pdf
;;
;; NOTE: maximum value of a hermitequintic interpolation with zero acceleration at the endpoints would be...
;; f(x=0.5) = MAXPOS + MAXVELOCITY * ( ( x - 6x^3 + 8x^4 - 3x^5 ) - ( -4x^3 + 7x^4 -3x^5 ) ) = MAXPOS + MAXVELOCITY * 0.3125
;;
;; variable naming conventions:
;; val = value ( position )
;; grad = gradient ( velocity )
;; x = 0.0->1.0 ( time )
;; i = interpolation = a value to be interpolated
;; e = evaluation = a value to be used to calculate the interpolation
;; 0 = start
;; 1 = end

(defun-g quintic-hermite
    ((x :float) (ival0 :float) (ival1 :float) (egrad0 :float) (egrad1 :float))
  (let* ((c0 (v3! (- 15.0) 8.0 7.0))
         (c1 (v3! 6.0 (- 3.0) (- 3.0)))
         (c2 (v3! 10.0 (- 6.0) (- 4.0)))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) (* x (* x x)))))
    (+ ival0
       (dot (v3! (- ival1 ival0) egrad0 egrad1)
            (+ (s~ h123 :xyz) (v3! 0.0 x 0.0))))))

(defun-g quintic-hermite
    ((x :float) (ival0 :vec4) (ival1 :vec4) (egrad0 :vec4) (egrad1 :vec4))
  (let* ((c0 (v3! (- 15.0) 8.0 7.0))
         (c1 (v3! 6.0 (- 3.0) (- 3.0)))
         (c2 (v3! 10.0 (- 6.0) (- 4.0)))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) (* x (* x x)))))
    (+ ival0
       (+ (* (- ival1 ival0) (s~ h123 :xxxx))
          (+ (* egrad0 (v4! (+ (y h123) x))) (* egrad1 (s~ h123 :zzzz)))))))

(defun-g quintic-hermite ((x :float)
                          (igrad0 :vec2) (igrad1 :vec2)
                          (egrad0 :vec2) (egrad1 :vec2))
  (let* ((c0 (v3! (- 15.0) 8.0 7.0))
         (c1 (v3! 6.0 (- 3.0) (- 3.0)))
         (c2 (v3! 10.0 (- 6.0) (- 4.0)))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) (* x (* x x)))))
    (+ (* (v! egrad1 igrad0)
          (v! (s~ h123 :zz) 1.0 1.0))
       (* (v! egrad0 (s~ h123 :xx))
          (v! (v2! (+ (y h123) x)) (- igrad1 igrad0))))))

(defun-g quintic-hermite ((x :float)
                          (ival0 :vec4) (ival1 :vec4)
                          (igrad-x0 :vec4) (igrad-x1 :vec4)
                          (igrad-y0 :vec4) (igrad-y1 :vec4)
                          (egrad0 :vec4) (egrad1 :vec4))
  (let* ((c0 (v3! (- 15.0) 8.0 7.0))
         (c1 (v3! 6.0 (- 3.0) (- 3.0)))
         (c2 (v3! 10.0 (- 6.0) (- 4.0)))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) (* x (* x x))))
         (out-ival
          (+ ival0
             (+ (* (- ival1 ival0) (s~ h123 :xxxx))
                (+ (* egrad0 (v4! (+ (y h123) x)))
                   (* egrad1 (s~ h123 :zzzz))))))
         (out-igrad-x (+ igrad-x0 (* (- igrad-x1 igrad-x0) (s~ h123 :xxxx))))
         (out-igrad-y
          (+ igrad-y0 (* (- igrad-y1 igrad-y0) (s~ h123 :xxxx)))))
    (values out-ival out-igrad-x out-igrad-y)))

(defun-g quintic-hermite ((x :float)
                          (igrad-x0 :vec4) (igrad-x1 :vec4) (igrad-y0 :vec4)
                          (igrad-y1 :vec4) (egrad0 :vec4) (egrad1 :vec4))
  (let* ((c0 (v3! (- 15.0) 8.0 7.0))
         (c1 (v3! 6.0 (- 3.0) (- 3.0)))
         (c2 (v3! 10.0 (- 6.0) (- 4.0)))
         (h123 (* (+ (* (+ c0 (* c1 x)) x) c2) (* x (* x x))))
         (out-ival
          (+ (* egrad0 (v4! (+ (y h123) x))) (* egrad1 (s~ h123 :zzzz))))
         (out-igrad-x (+ igrad-x0 (* (- igrad-x1 igrad-x0) (s~ h123 :xxxx))))
         (out-igrad-y
          (+ igrad-y0 (* (- igrad-y1 igrad-y0) (s~ h123 :xxxx)))))
    (values out-ival out-igrad-x out-igrad-y)))

(defun-g quintic-hermite-deriv ((x :float)
                                (ival0 :float) (ival1 :float)
                                (egrad0 :float) (egrad1 :float))
  (let* ((c0 (v3! 30.0 (- 15.0) (- 15.0)))
         (c1 (v3! (- 60.0) 32.0 28.0))
         (c2 (v3! 30.0 (- 18.0) (- 12.0)))
         (h123 (* (+ (* (+ c1 (* c0 x)) x) c2) (* x x))))
    (dot (v3! (- ival1 ival0) egrad0 egrad1)
         (+ (s~ h123 :xyz) (v3! 0.0 1.0 0.0)))))
