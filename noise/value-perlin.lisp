(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

(defun-g value-perlin-noise ((p :vec2) (blend-val :float))
  (let* ((pi (floor p))
         (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0))))))
    (multiple-value-bind (hash-value hash-x hash-y)
        (bs-fast32-hash-3-per-corner pi)
      (let* ((grad-x (- hash-x (v4! "0.49999")))
             (grad-y (- hash-y (v4! "0.49999")))
             (grad-results
              (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                 (+ (* grad-x (s~ pf-pfmin1 :xzxz))
                    (* grad-y (s~ pf-pfmin1 :yyww))))))
        (multf grad-results (v4! "1.4142135623730950488016887242097"))
        (setf grad-results
              (mix (- (* hash-value 2.0) (v4! 1.0)) grad-results blend-val))
        (let* ((blend (perlin-quintic (s~ pf-pfmin1 :xy)))
               (blend2 (v! blend (- (v2! 1.0) blend))))
          (dot grad-results (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))))

;;------------------------------------------------------------
;; 3D

(defun-g value-perlin-noise ((p :vec3) (blend-val :float))
  (let* ((pi (floor p))
         (pf (- p pi))
         (pf-min1 (- pf (v3! 1.0))))
    (multiple-value-bind (hash-value0
                          hashx0 hashy0 hashz0
                          hash-value1
                          hashx1 hashy1 hashz1)
        (bs-fast32-hash-4-per-corner pi)
      (let* ((grad-x0 (- hashx0 (v4! "0.49999")))
             (grad-y0 (- hashy0 (v4! "0.49999")))
             (grad-z0 (- hashz0 (v4! "0.49999")))
             (grad-x1 (- hashx1 (v4! "0.49999")))
             (grad-y1 (- hashy1 (v4! "0.49999")))
             (grad-z1 (- hashz1 (v4! "0.49999")))
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
        (multf grad-results-0 (v4! "1.1547005383792515290182975610039"))
        (multf grad-results-1 (v4! "1.1547005383792515290182975610039"))
        (setf grad-results-0
              (mix (- (* hash-value0 2.0) (v4! 1.0)) grad-results-0 blend-val))
        (setf grad-results-1
              (mix (- (* hash-value1 2.0) (v4! 1.0)) grad-results-1 blend-val))
        (let* ((blend (perlin-quintic pf))
               (res0 (mix grad-results-0 grad-results-1 (z blend)))
               (blend2 (v! (s~ blend :xy) (- (v2! 1.0) (s~ blend :xy)))))
          (dot res0 (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))))
