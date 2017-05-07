(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

(defun-g value-hermite-noise ((p :vec2)
                              (value-scale :float)
                              (gradient-scale :float)
                              (normalization-val :float))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-value hash-gradx hash-grady)
        (bs-fast32-hash-3-per-corner pi)
      (setf hash-gradx (* (- hash-gradx (v4! 0.49999)) gradient-scale))
      (setf hash-grady (* (- hash-grady (v4! 0.49999)) gradient-scale))
      (setf hash-value (* (- hash-value (v4! 0.5)) value-scale))
      (let* ((qh-results
              (quintic-hermite (y pf)
                               (v! (s~ hash-value :xy) (s~ hash-gradx :xy))
                               (v! (s~ hash-value :zw) (s~ hash-gradx :zw))
                               (v! (s~ hash-grady :xy) 0.0 0.0)
                               (v! (s~ hash-grady :zw) 0.0 0.0))))
        (* (quintic-hermite (x pf) (x qh-results) (y qh-results) (z qh-results)
                            (w qh-results))
           normalization-val)))))

;;------------------------------------------------------------
;; 3D

(defun-g value-hermite-noise ((p :vec3)
                              (value-scale :float)
                              (gradient-scale :float)
                              (normalization-val :float))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-value0
                          hash-gradx0 hash-grady0 hash-gradz0
                          hash-value1
                          hash-gradx1 hash-grady1 hash-gradz1)
        (bs-fast32-hash-4-per-corner pi)
      (setf hash-gradx0 (* (- hash-gradx0 (v4! 0.49999)) gradient-scale))
      (setf hash-grady0 (* (- hash-grady0 (v4! 0.49999)) gradient-scale))
      (setf hash-gradz0 (* (- hash-gradz0 (v4! 0.49999)) gradient-scale))
      (setf hash-gradx1 (* (- hash-gradx1 (v4! 0.49999)) gradient-scale))
      (setf hash-grady1 (* (- hash-grady1 (v4! 0.49999)) gradient-scale))
      (setf hash-gradz1 (* (- hash-gradz1 (v4! 0.49999)) gradient-scale))
      (setf hash-value0 (* (- hash-value0 (v4! 0.5)) value-scale))
      (setf hash-value1 (* (- hash-value1 (v4! 0.5)) value-scale))
      (multiple-value-bind (ival-results igrad-results-x igrad-results-y)
          (quintic-hermite (z pf)
                           hash-value0 hash-value1 hash-gradx0 hash-gradx1
                           hash-grady0 hash-grady1 hash-gradz0 hash-gradz1)
        (let* ((qh-results
                (quintic-hermite (y pf)
                                 (v! (s~ ival-results :xy) (s~ igrad-results-x :xy))
                                 (v! (s~ ival-results :zw) (s~ igrad-results-x :zw))
                                 (v! (s~ igrad-results-y :xy) 0.0 0.0)
                                 (v! (s~ igrad-results-y :zw) 0.0 0.0))))
          (*
           (quintic-hermite (x pf) (x qh-results) (y qh-results) (z qh-results)
                            (w qh-results))
           normalization-val))))))
