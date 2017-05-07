(in-package :nineveh.noise)

;;----------------------------------------------------------------------

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

;;----------------------------------------------------------------------
