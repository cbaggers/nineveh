(in-package :nineveh.noise)

;;----------------------------------------------------------------------
;; 2D

(defun-g polka-dot-noise ((p :vec2) (radius-low :float) (radius-high :float))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (bs-fast32-hash-cell pi))
         (radius
          (max 0.0 (+ radius-low (* (z hash) (- radius-high radius-low)))))
         (value (/ radius (max radius-high radius-low))))
    (setf radius (/ 2.0 radius))
    (multf pf (v2! radius))
    (decf pf (v2! (- radius 1.0)))
    (incf pf (* (s~ hash :xy) (- radius 2.0)))
    (* (falloff-xsq-c2 (min (dot pf pf) 1.0)) value)))

(defun-g polka-dot-noise-simplex ((p :vec2)
                                  (radius :float)
                                  (max-dimness :float))
  (let* ((skew-factor 0.36602542)
         (unskew-factor 0.21132489)
         (simplex-tri-height 0.7071068)
         (inv-simplex-tri-half-edgelen 2.4494898)
         (simplex-points
          (v3! (- 1.0 unskew-factor) (- unskew-factor)
               (- 1.0 (* 2.0 unskew-factor)))))
    (multf p (v2! simplex-tri-height))
    (let* ((pi (floor (+ p (v2! (dot p (v2! skew-factor))))))
           (v0 (- pi (- (v2! (dot pi (v2! unskew-factor))) p)))
           (v0123-x (+ (v! 0.0 (s~ simplex-points :xyz)) (v4! (x v0))))
           (v0123-y (+ (v! 0.0 (s~ simplex-points :yxz)) (v4! (y v0))))
           (hash (bs-fast32-hash pi)))
      (setf radius (/ inv-simplex-tri-half-edgelen radius))
      (multf v0123-x (v4! radius))
      (multf v0123-y (v4! radius))
      (let* ((point-distance
              (max (v4! 0.0)
                   (- (v4! 1.0) (+ (* v0123-x v0123-x) (* v0123-y v0123-y))))))
        (setf point-distance
              (* point-distance (* point-distance point-distance)))
        (dot (- (v4! 1.0) (* hash max-dimness)) point-distance)))))

(defun-g stars-noise ((p :vec2)
                      (probability-threshold :float)
                      (max-dimness :float)
                      (two-over-radius :float))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (bs-fast32-hash-cell pi))
         (value (- 1.0 (* max-dimness (z hash)))))
    (multf pf (v2! two-over-radius))
    (decf pf (v2! (- two-over-radius 1.0)))
    (incf pf (* (s~ hash :xy) (- two-over-radius 2.0)))
    (if (< (w hash) probability-threshold)
        (* (falloff-xsq-c1 (min (dot pf pf) 1.0)) value)
        0.0)))

;;----------------------------------------------------------------------

(defun-g polka-dot-noise ((p :vec3) (radius-low :float) (radius-high :float))
  (let* ((pi (floor p))
         (pf (- p pi))
         (hash (bs-fast32-hash-cell pi))
         (radius
          (max 0.0 (+ radius-low (* (w hash) (- radius-high radius-low)))))
         (value (/ radius (max radius-high radius-low))))
    (setf radius (/ 2.0 radius))
    (multf pf (v3! radius))
    (decf pf (v3! (- radius 1.0)))
    (incf pf (* (s~ hash :xyz) (- radius 2.0)))
    (* (falloff-xsq-c2 (min (dot pf pf) 1.0)) value)))


(defun-g polka-dot-noise-simplex ((p :vec3) (radius :float) (max-dimness :float))
  (multiple-value-bind (pi pi-1 pi-2 v1234-x v1234-y v1234-z)
      (simplex-3d-get-corner-vectors p)
    (let* ((hash (bs-fast32-hash pi pi-1 pi-2))
           (inv-simplex-tri-half-edgelen 2.309401))
      (setf radius (/ inv-simplex-tri-half-edgelen radius))
      (multf v1234-x (v4! radius))
      (multf v1234-y (v4! radius))
      (multf v1234-z (v4! radius))
      (let* ((point-distance
              (max (v4! 0.0)
                   (- (v4! 1.0)
                      (+ (* v1234-x v1234-x)
                         (+ (* v1234-y v1234-y) (* v1234-z v1234-z)))))))
        (setf point-distance
              (* point-distance (* point-distance point-distance)))
        (dot (- (v4! 1.0) (* hash max-dimness)) point-distance)))))

;;----------------------------------------------------------------------
