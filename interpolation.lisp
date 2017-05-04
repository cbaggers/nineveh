(in-package :nineveh.hashing)

(defun-g interpolation-c1 ((x :float))
  (* x (* x (- 3.0 (* 2.0 x)))))

(defun-g interpolation-c1 ((x :vec2))
  (* x (* x (- (v2! 3.0) (* 2.0 x)))))

(defun-g interpolation-c1 ((x :vec3))
  (* x (* x (- (v3! 3.0) (* 2.0 x)))))

(defun-g interpolation-c1 ((x :vec4))
  (* x (* x (- (v4! 3.0) (* 2.0 x)))))

(defun-g interpolation-c2 ((x :float))
  (* x (* x (* x (+ (* x (- (* x 6.0) 15.0)) 10.0)))))

(defun-g interpolation-c2 ((x :vec2))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v2! 15.0)))
                    (v2! 10.0))))))

(defun-g interpolation-c2 ((x :vec3))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v3! 15.0)))
                    (v3! 10.0))))))

(defun-g interpolation-c2 ((x :vec4))
  (* x (* x (* x (+ (* x (- (* x 6.0) (v4! 15.0)))
                    (v4! 10.0))))))

(defun-g interpolation-c2-interp-and-deriv ((x :vec2))
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

(defun-g interpolation-c2-deriv ((x :vec3))
  (* x (* x (+ (* x (- (* x 30.0) (v3! 60.0)))
               (v3! 30.0)))))

(defun-g interpolation-c2-fast ((x :float))
  (let* ((x3 (* x (* x x))))
    (* (+ 7.0 (* (- x3 7.0) x)) x3)))

(defun-g interpolation-c2-fast ((x :vec2))
  (let* ((x3 (* x (* x x))))
    (* (+ (v2! 7.0) (* (- x3 (v2! 7.0)) x)) x3)))

(defun-g interpolation-c2-fast ((x :vec3))
  (let* ((x3 (* x (* x x))))
    (* (+ (v3! 7.0) (* (- x3 (v3! 7.0)) x)) x3)))

(defun-g interpolation-c2-fast ((x :vec4))
  (let* ((x3 (* x (* x x))))
    (* (+ (v4! 7.0) (* (- x3 (v4! 7.0)) x)) x3)))

(defun-g interpolation-c3 ((x :float))
  (let* ((xsq (* x x)) (xsqsq (* xsq xsq)))
    (* xsqsq (- 25.0 (+ (* 48.0 x) (* xsq (- 25.0 xsqsq)))))))

(defun-g interpolation-c3 ((x :vec2))
  (let* ((xsq (* x x)) (xsqsq (* xsq xsq)))
    (* xsqsq (- (v2! 25.0)
                (+ (* (v2! 48.0) x)
                   (* xsq (- (v2! 25.0) xsqsq)))))))

(defun-g interpolation-c3 ((x :vec3))
  (let* ((xsq (* x x)) (xsqsq (* xsq xsq)))
    (* xsqsq (- (v3! 25.0)
                (+ (* (v3! 48.0) x)
                   (* xsq (- (v3! 25.0) xsqsq)))))))

(defun-g interpolation-c3 ((x :vec4))
  (let* ((xsq (* x x)) (xsqsq (* xsq xsq)))
    (* xsqsq (- (v4! 25.0)
                (+ (* (v4! 48.0) x)
                   (* xsq (- (v4! 25.0) xsqsq)))))))
