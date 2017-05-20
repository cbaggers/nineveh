(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

(defun-g value-noise ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (bs-fast32-hash pi))
         (blend (quintic pf))
         (blend2 (v! blend (- (v2! 1.0) blend))))
    (dot hash (* (s~ blend2 :zxzx) (s~ blend2 :wwyy)))))

(defun-g value-noise-deriv ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (bs-fast32-hash pi))
         (blend (quintic-interp-and-deriv pf))
         (res0 (mix (s~ hash :xyxz) (s~ hash :zwyw) (s~ blend :yyxx))))
    (+ (v3! (x res0) 0.0 0.0)
       (* (- (s~ res0 :yyw) (s~ res0 :xxz)) (s~ blend :xzw)))))

;;------------------------------------------------------------
;; 3D

(defun-g value-noise ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-lowz hash-highz) (bs-fast32-hash pi)
      (let* ((blend (quintic pf))
             (res0 (mix hash-lowz hash-highz (z blend)))
             (blend2 (v! (s~ blend :xy) (- (v2! 1.0) (s~ blend :xy)))))
        (dot res0 (* (s~ blend2 :zxzx) (s~ blend2 :wwyy)))))))

(defun-g value-noise-deriv ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-lowz hash-highz)
        (bs-fast32-hash pi)
      (let* ((blend (quintic pf))
             (res0 (mix hash-lowz hash-highz (z blend)))
             (res1 (mix (s~ res0 :xyxz) (s~ res0 :zwyw) (s~ blend :yyxx)))
             (res3
              (mix (v! (s~ hash-lowz :xy) (s~ hash-highz :xy))
                   (v! (s~ hash-lowz :zw) (s~ hash-highz :zw)) (y blend)))
             (res4 (mix (s~ res3 :xz) (s~ res3 :yw) (x blend))))
        (+ (v4! (x res1) 0.0 0.0 0.0)
           (* (- (v! (s~ res1 :yyw) (y res4))
                 (v! (s~ res1 :xxz) (x res4)))
              (v! (x blend) (quintic-deriv pf))))))))

;;------------------------------------------------------------
;; 4D

(defun-g value-noise ((p :vec4))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (z0w0-hash z1w0-hash z0w1-hash z1w1-hash)
        (bs-quick32-hash pi)
      (let* ((blend (quintic pf))
             (res0 (+ z0w0-hash (* (- z0w1-hash z0w0-hash) (s~ blend :wwww))))
             (res1 (+ z1w0-hash (* (- z1w1-hash z1w0-hash) (s~ blend :wwww)))))
        (setf res0 (+ res0 (* (- res1 res0) (s~ blend :zzzz))))
        (setf (s~ blend :zw) (- (v2! 1.0) (s~ blend :xy)))
        (dot res0 (* (s~ blend :zxzx) (s~ blend :wwyy)))))))
