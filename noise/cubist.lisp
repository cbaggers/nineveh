(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

(defun-g cubist-noise ((p :vec2) (range-clamp :vec2))
  (let* ((pi (floor p))
         (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0))))))
    (multiple-value-bind (hash-x hash-y hash-value)
        (bs-fast32-hash-3-per-corner pi)
      (let* ((grad-x (- hash-x (v4! 0.49999)))
             (grad-y (- hash-y (v4! 0.49999)))
             (grad-results
              (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                 (+ (* grad-x (s~ pf-pfmin1 :xzxz))
                    (* grad-y (s~ pf-pfmin1 :yyww))))))
        (setf grad-results (* (- hash-value (v4! 0.5))
                              (/ (v4! 1.0) grad-results)))
        (let* ((blend (perlin-quintic (s~ pf-pfmin1 :xy)))
               (blend2 (v! blend (- (v2! 1.0) blend)))
               (final (dot grad-results (* (s~ blend2 :zxzx)
                                           (s~ blend2 :wwyy)))))
          (clamp (* (- final (x range-clamp)) (y range-clamp)) 0.0 1.0))))))

;;------------------------------------------------------------
;; 3D

(defun-g cubist-noise ((p :vec3) (range-clamp :vec2))
  (let* ((pi (floor p))
         (pf (- p pi))
         (pf-min1 (- pf (v3! 1.0))))
    (multiple-value-bind (hashx0 hashy0 hashz0
                                 hash-value0
                                 hashx1 hashy1 hashz1
                                 hash-value1)
        (bs-fast32-hash-4-per-corner pi)
      (let* ((grad-x0 (- hashx0 (v4! 0.49999)))
             (grad-y0 (- hashy0 (v4! 0.49999)))
             (grad-z0 (- hashz0 (v4! 0.49999)))
             (grad-x1 (- hashx1 (v4! 0.49999)))
             (grad-y1 (- hashy1 (v4! 0.49999)))
             (grad-z1 (- hashz1 (v4! 0.49999)))
             (grad-results-0
              (*
               (inversesqrt
                (+ (* grad-x0 grad-x0)
                   (+ (* grad-y0 grad-y0) (* grad-z0 grad-z0))))
               (+ (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) grad-x0)
                  (+ (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) grad-y0)
                     (* (s~ pf :zzzz) grad-z0)))))
             (grad-results-1
              (*
               (inversesqrt
                (+ (* grad-x1 grad-x1)
                   (+ (* grad-y1 grad-y1) (* grad-z1 grad-z1))))
               (+ (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) grad-x1)
                  (+ (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) grad-y1)
                     (* (s~ pf-min1 :zzzz) grad-z1))))))
        (setf grad-results-0 (* (- hash-value0 (v4! 0.5))
                                (/ (v4! 1.0) grad-results-0)))
        (setf grad-results-1 (* (- hash-value1 (v4! 0.5))
                                (/ (v4! 1.0) grad-results-1)))
        (let* ((blend (perlin-quintic pf))
               (res0 (mix grad-results-0 grad-results-1 (z blend)))
               (blend2 (v! (s~ blend :xy) (- (v2! 1.0) (s~ blend :xy))))
               (final (dot res0 (* (s~ blend2 :zxzx) (s~ blend2 :wwyy)))))
          (clamp (* (- final (x range-clamp)) (y range-clamp)) 0.0 1.0))))))
