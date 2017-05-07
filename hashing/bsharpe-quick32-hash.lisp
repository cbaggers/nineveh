(in-package :nineveh.hashing)

;;
;; bs-quick32-hash (known in Brian's work as FastHash32_2)
;;
;; An alternative to FastHash32
;; - slightly slower
;; - can have a larger domain
;; - allows for a 4D implementation
;;

;;------------------------------------------------------------
;; 2D

(defun-g bs-quick32-hash ((grid-cell :vec2))
  (let* ((offset (glsl-expr "vec2(403.839172, 377.242706)" :vec2))
         (domain 69.0)
         (somelargefloat (glsl-expr "32745.708984" :float))
         (scale (glsl-expr "vec2(2.009842, 1.372549)" :vec2))
         (p (v! (s~ grid-cell :xy) (+ (s~ grid-cell :xy) (v2! 1.0)))))
    (setf p (- p (* (floor (* p (/ 1.0 domain))) domain)))
    (setf p (+ (* p (s~ scale :xyxy)) (s~ offset :xyxy)))
    (multf p p)
    (fract (* (s~ p :xzxz) (* (s~ p :yyww) (/ 1.0 somelargefloat))))))

;;------------------------------------------------------------
;; 3D

(defun-g bs-quick32-hash ((grid-cell :vec3))
  (let (((z0-hash :vec4)) ((z1-hash :vec4)))
    (let* ((offset (glsl-expr "vec3(55.882355, 63.167774, 52.941177)" :vec3))
           (domain 69.0)
           (somelargefloat (glsl-expr "69412.070313" :float))
           (scale (glsl-expr "vec3(0.235142, 0.205890, 0.216449)" :vec3)))
      (setf grid-cell
            (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v3! (- domain 1.5))) (+ grid-cell (v3! 1.0)))))
        (setf grid-cell (+ (* grid-cell scale) offset))
        (setf grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
        (multf grid-cell grid-cell)
        (multf grid-cell-inc1 grid-cell-inc1)
        (let* ((x0y0-x1y0-x0y1-x1y1
                (*
                 (v4! (x grid-cell) (x grid-cell-inc1) (x grid-cell)
                      (x grid-cell-inc1))
                 (v! (s~ grid-cell :yy) (s~ grid-cell-inc1 :yy)))))
          (setf z0-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1
                    (* (s~ grid-cell :zzzz) (/ 1.0 somelargefloat)))))
          (setf z1-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1
                    (* (s~ grid-cell-inc1 :zzzz)
                       (/ 1.0 somelargefloat))))))))
    (values z0-hash z1-hash)))

;;------------------------------------------------------------
;; 4D

(defun-g bs-quick32-hash ((grid-cell :vec4))
  (let (((z0w0-hash :vec4))
        ((z1w0-hash :vec4))
        ((z0w1-hash :vec4))
        ((z1w1-hash :vec4)))
    (let* ((offset (glsl-expr "vec4(16.84123, 18.774548, 16.873274, 13.664607)"
                              :vec4))
           (domain 69.0)
           (somelargefloat (glsl-expr "47165.636719" :float))
           (scale (glsl-expr "vec4(0.102007, 0.114473, 0.139651, 0.084550)"
                             :vec4)))
      (setf grid-cell
            (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v4! (- domain 1.5))) (+ grid-cell (v4! 1.0)))))
        (setf grid-cell (+ (* grid-cell scale) offset))
        (setf grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
        (multf grid-cell grid-cell)
        (multf grid-cell-inc1 grid-cell-inc1)
        (let* ((x0y0-x1y0-x0y1-x1y1
                (*
                 (v4! (x grid-cell) (x grid-cell-inc1) (x grid-cell)
                      (x grid-cell-inc1))
                 (v! (s~ grid-cell :yy) (s~ grid-cell-inc1 :yy))))
               (z0w0-z1w0-z0w1-z1w1
                (*
                 (v4! (z grid-cell) (z grid-cell-inc1) (z grid-cell)
                      (z grid-cell-inc1))
                 (* (v! (s~ grid-cell :ww) (s~ grid-cell-inc1 :ww))
                    (/ 1.0 somelargefloat)))))
          (setf z0w0-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :xxxx))))
          (setf z1w0-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :yyyy))))
          (setf z0w1-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :zzzz))))
          (setf z1w1-hash
                (fract
                 (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :wwww)))))))
    (values z0w0-hash z1w0-hash z0w1-hash z1w1-hash)))

(defun-g bs-quick32-hash-4-per-corner ((grid-cell :vec4))
  (let (((z0w0-hash-0 :vec4))
        ((z0w0-hash-1 :vec4))
        ((z0w0-hash-2 :vec4))
        ((z0w0-hash-3 :vec4))
        ((z1w0-hash-0 :vec4))
        ((z1w0-hash-1 :vec4))
        ((z1w0-hash-2 :vec4))
        ((z1w0-hash-3 :vec4))
        ((z0w1-hash-0 :vec4))
        ((z0w1-hash-1 :vec4))
        ((z0w1-hash-2 :vec4))
        ((z0w1-hash-3 :vec4))
        ((z1w1-hash-0 :vec4))
        ((z1w1-hash-1 :vec4))
        ((z1w1-hash-2 :vec4))
        ((z1w1-hash-3 :vec4)))
    (let* ((offset (glsl-expr "vec4(16.84123, 18.774548, 16.873274, 13.664607)"
                              :vec4))
           (domain 69.0)
           (some-large-floats
            (glsl-expr "vec4(56974.746094, 47165.636719, 55049.667969, 49901.273438)"
                       :vec4))
           (scale (glsl-expr "vec4(0.102007, 0.114473, 0.139651, 0.084550)"
                             :vec4)))
      (setf grid-cell
            (- grid-cell (* (floor (* grid-cell (/ 1.0 domain))) domain)))
      (let* ((grid-cell-inc1
              (* (step grid-cell (v4! (- domain 1.5))) (+ grid-cell (v4! 1.0)))))
        (setf grid-cell (+ (* grid-cell scale) offset))
        (setf grid-cell-inc1 (+ (* grid-cell-inc1 scale) offset))
        (multf grid-cell grid-cell)
        (multf grid-cell-inc1 grid-cell-inc1)
        (let* ((x0y0-x1y0-x0y1-x1y1
                (*
                 (v4! (x grid-cell) (x grid-cell-inc1) (x grid-cell)
                      (x grid-cell-inc1))
                 (v! (s~ grid-cell :yy) (s~ grid-cell-inc1 :yy))))
               (z0w0-z1w0-z0w1-z1w1
                (*
                 (v4! (z grid-cell) (z grid-cell-inc1) (z grid-cell)
                      (z grid-cell-inc1))
                 (v! (s~ grid-cell :ww) (s~ grid-cell-inc1 :ww))))
               (hashval (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :xxxx))))
          (setf z0w0-hash-0 (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z0w0-hash-1 (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z0w0-hash-2 (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z0w0-hash-3 (fract (* hashval (/ 1.0 (w some-large-floats)))))
          (setf hashval
                (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :yyyy)))
          (setf z1w0-hash-0 (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z1w0-hash-1 (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z1w0-hash-2
                (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z1w0-hash-3
                (fract (* hashval (/ 1.0 (w some-large-floats)))))
          (setf hashval
                (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :zzzz)))
          (setf z0w1-hash-0
                (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z0w1-hash-1
                (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z0w1-hash-2
                (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z0w1-hash-3
                (fract (* hashval (/ 1.0 (w some-large-floats)))))
          (setf hashval
                (* x0y0-x1y0-x0y1-x1y1 (s~ z0w0-z1w0-z0w1-z1w1 :wwww)))
          (setf z1w1-hash-0
                (fract (* hashval (/ 1.0 (x some-large-floats)))))
          (setf z1w1-hash-1
                (fract (* hashval (/ 1.0 (y some-large-floats)))))
          (setf z1w1-hash-2
                (fract (* hashval (/ 1.0 (z some-large-floats)))))
          (setf z1w1-hash-3
                (fract
                 (* hashval (/ 1.0 (w some-large-floats))))))))
    (values z0w0-hash-0 z0w0-hash-1 z0w0-hash-2 z0w0-hash-3 z1w0-hash-0
            z1w0-hash-1 z1w0-hash-2 z1w0-hash-3 z0w1-hash-0 z0w1-hash-1
            z0w1-hash-2 z0w1-hash-3 z1w1-hash-0 z1w1-hash-1 z1w1-hash-2
            z1w1-hash-3)))

;;------------------------------------------------------------
