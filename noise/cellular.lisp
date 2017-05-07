(in-package :nineveh.noise)

;;----------------------------------------------------------------------

(defun-g cellular-weight-samples ((samples :vec4))
  (setf samples (- (* samples 2.0) (v4! 1.0)))
  (- (* samples (* samples samples)) (sign samples)))

;;----------------------------------------------------------------------
;; 2D

(defun-g cellular-noise ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-x hash-y) (bs-fast32-hash-2-per-corner pi)
      (let* ((jitter-window 0.25))
        (setf hash-x
              (+ (* (cellular-weight-samples hash-x) jitter-window)
                 (v4! 0.0 1.0 0.0 1.0)))
        (setf hash-y
              (+ (* (cellular-weight-samples hash-y) jitter-window)
                 (v4! 0.0 0.0 1.0 1.0)))
        (let* ((dx (- (s~ pf :xxxx) hash-x))
               (dy (- (s~ pf :yyyy) hash-y))
               (d (+ (* dx dx) (* dy dy))))
          (setf (s~ d :xy) (min (s~ d :xy) (s~ d :zw)))
          (* (min (x d) (y d)) (/ 1.0 1.125)))))))

(defun-g cellular-noise-fast ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-x hash-y) (bs-fast32-hash-2-per-corner pi)
      (let* ((jitter-window 0.4))
        (setf hash-x
              (+ (* hash-x (* jitter-window 2.0))
                 (v4! (- jitter-window) (- 1.0 jitter-window)
                      (- jitter-window) (- 1.0 jitter-window))))
        (setf hash-y
              (+ (* hash-y (* jitter-window 2.0))
                 (v4! (- jitter-window) (- jitter-window)
                      (- 1.0 jitter-window) (- 1.0 jitter-window))))
        (let* ((dx (- (s~ pf :xxxx) hash-x))
               (dy (- (s~ pf :yyyy) hash-y))
               (d (+ (* dx dx) (* dy dy))))
          (setf (s~ d :xy) (min (s~ d :xy) (s~ d :zw)))
          (* (min (x d) (y d)) (/ 1.0 1.125)))))))

;;----------------------------------------------------------------------
;; 3D

(defun-g cellular-noise ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-x0 hash-y0 hash-z0 hash-x1 hash-y1 hash-z1)
        (bs-fast32-hash-3-per-corner pi)
      (let* ((jitter-window 0.16666667))
        (setf hash-x0
              (+ (* (cellular-weight-samples hash-x0) jitter-window)
                 (v4! 0.0 1.0 0.0 1.0)))
        (setf hash-y0
              (+ (* (cellular-weight-samples hash-y0) jitter-window)
                 (v4! 0.0 0.0 1.0 1.0)))
        (setf hash-x1
              (+ (* (cellular-weight-samples hash-x1) jitter-window)
                 (v4! 0.0 1.0 0.0 1.0)))
        (setf hash-y1
              (+ (* (cellular-weight-samples hash-y1) jitter-window)
                 (v4! 0.0 0.0 1.0 1.0)))
        (progn
          (setf hash-z0
                (+ (* (cellular-weight-samples hash-z0) jitter-window)
                   (v4! 0.0 0.0 0.0 0.0)))
          (setf hash-z1
                (+ (* (cellular-weight-samples hash-z1) jitter-window)
                   (v4! 1.0 1.0 1.0 1.0)))
          (let* ((dx1 (- (s~ pf :xxxx) hash-x0))
                 (dy1 (- (s~ pf :yyyy) hash-y0))
                 (dz1 (- (s~ pf :zzzz) hash-z0))
                 (dx2 (- (s~ pf :xxxx) hash-x1))
                 (dy2 (- (s~ pf :yyyy) hash-y1))
                 (dz2 (- (s~ pf :zzzz) hash-z1))
                 (d1 (+ (* dx1 dx1) (+ (* dy1 dy1) (* dz1 dz1))))
                 (d2 (+ (* dx2 dx2) (+ (* dy2 dy2) (* dz2 dz2)))))
            (setf d1 (min d1 d2))
            (setf (s~ d1 :xy) (min (s~ d1 :xy) (s~ d1 :wz)))
            (* (min (x d1) (y d1)) (/ 9.0 12.0))))))))

(defun-g cellular-noise-fast ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-x0 hash-y0 hash-z0 hash-x1 hash-y1 hash-z1)
        (bs-fast32-hash-3-per-corner pi)
      (let* ((jitter-window 0.4))
        (setf hash-x0
              (+ (* hash-x0 (* jitter-window 2.0))
                 (v4! (- jitter-window) (- 1.0 jitter-window)
                      (- jitter-window) (- 1.0 jitter-window))))
        (setf hash-y0
              (+ (* hash-y0 (* jitter-window 2.0))
                 (v4! (- jitter-window) (- jitter-window)
                      (- 1.0 jitter-window) (- 1.0 jitter-window))))
        (setf hash-x1
              (+ (* hash-x1 (* jitter-window 2.0))
                 (v4! (- jitter-window) (- 1.0 jitter-window)
                      (- jitter-window) (- 1.0 jitter-window))))
        (setf hash-y1
              (+ (* hash-y1 (* jitter-window 2.0))
                 (v4! (- jitter-window) (- jitter-window)
                      (- 1.0 jitter-window) (- 1.0 jitter-window))))
        (progn
          (setf hash-z0
                (+ (* hash-z0 (* jitter-window 2.0))
                   (v4! (- jitter-window) (- jitter-window)
                        (- jitter-window) (- jitter-window))))
          (setf hash-z1
                (+ (* hash-z1 (* jitter-window 2.0))
                   (v4! (- 1.0 jitter-window) (- 1.0 jitter-window)
                        (- 1.0 jitter-window) (- 1.0 jitter-window))))
          (let* ((dx1 (- (s~ pf :xxxx) hash-x0))
                 (dy1 (- (s~ pf :yyyy) hash-y0))
                 (dz1 (- (s~ pf :zzzz) hash-z0))
                 (dx2 (- (s~ pf :xxxx) hash-x1))
                 (dy2 (- (s~ pf :yyyy) hash-y1))
                 (dz2 (- (s~ pf :zzzz) hash-z1))
                 (d1 (+ (* dx1 dx1) (+ (* dy1 dy1) (* dz1 dz1))))
                 (d2 (+ (* dx2 dx2) (+ (* dy2 dy2) (* dz2 dz2)))))
            (setf d1 (min d1 d2))
            (setf (s~ d1 :xy) (min (s~ d1 :xy) (s~ d1 :wz)))
            (* (min (x d1) (y d1)) (/ 9.0 12.0))))))))

;;----------------------------------------------------------------------
