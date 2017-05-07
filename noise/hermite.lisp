(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

(defun-g hermite-noise ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx hash-grady)
        (bs-fast32-hash-2-per-corner pi)
      (setf hash-gradx (- hash-gradx (v4! 0.49999)))
      (setf hash-grady (- hash-grady (v4! 0.49999)))
      (let* ((norm
              (inversesqrt
               (+ (* hash-gradx hash-gradx) (* hash-grady hash-grady)))))
        (multf hash-gradx norm)
        (multf hash-grady norm)
        (let* ((final-norm-val 2.2627418)
               (qh-results
                (quintic-hermite (y pf) (s~ hash-gradx :xy) (s~ hash-gradx :zw)
                                 (s~ hash-grady :xy) (s~ hash-grady :zw))))
          (* (quintic-hermite (x pf)
                              (x qh-results) (y qh-results)
                              (z qh-results) (w qh-results))
             final-norm-val))))))

(defun-g hermite-noise-unnormalized-gradients ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx hash-grady)
        (bs-fast32-hash-2-per-corner pi)
      (setf hash-gradx (- hash-gradx (v4! 0.49999)))
      (setf hash-grady (- hash-grady (v4! 0.49999)))
      (let* ((final-norm-val 3.2)
             (qh-results
              (quintic-hermite (y pf) (s~ hash-gradx :xy) (s~ hash-gradx :zw)
                               (s~ hash-grady :xy) (s~ hash-grady :zw))))
        (*
         (quintic-hermite (x pf) (x qh-results) (y qh-results) (z qh-results)
                          (w qh-results))
         final-norm-val)))))

;;------------------------------------------------------------
;; 3D

(defun-g hermite-noise ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx0
                          hash-grady0
                          hash-gradz0
                          hash-gradx1
                          hash-grady1
                          hash-gradz1)
        (bs-fast32-hash-3-per-corner pi)
      (setf hash-gradx0 (- hash-gradx0 (v4! 0.49999)))
      (setf hash-grady0 (- hash-grady0 (v4! 0.49999)))
      (setf hash-gradz0 (- hash-gradz0 (v4! 0.49999)))
      (setf hash-gradx1 (- hash-gradx1 (v4! 0.49999)))
      (setf hash-grady1 (- hash-grady1 (v4! 0.49999)))
      (setf hash-gradz1 (- hash-gradz1 (v4! 0.49999)))
      (let* ((norm0
              (inversesqrt
               (+ (* hash-gradx0 hash-gradx0)
                  (+ (* hash-grady0 hash-grady0)
                     (* hash-gradz0 hash-gradz0))))))
        (multf hash-gradx0 norm0)
        (multf hash-grady0 norm0)
        (multf hash-gradz0 norm0)
        (let* ((norm1
                (inversesqrt
                 (+ (* hash-gradx1 hash-gradx1)
                    (+ (* hash-grady1 hash-grady1)
                       (* hash-gradz1 hash-gradz1))))))
          (multf hash-gradx1 norm1)
          (multf hash-grady1 norm1)
          (multf hash-gradz1 norm1)
          (let* ((final-norm-val 1.8475208))
            (multiple-value-bind (ival-results igrad-results-x igrad-results-y)
                (quintic-hermite (z pf)
                                 hash-gradx0 hash-gradx1 hash-grady0
                                 hash-grady1 hash-gradz0 hash-gradz1)
              (let* ((qh-results
                      (quintic-hermite
                       (y pf)
                       (v! (s~ ival-results :xy) (s~ igrad-results-x :xy))
                       (v! (s~ ival-results :zw) (s~ igrad-results-x :zw))
                       (v! (s~ igrad-results-y :xy) 0.0 0.0)
                       (v! (s~ igrad-results-y :zw) 0.0 0.0))))
                (*
                 (quintic-hermite (x pf) (x qh-results) (y qh-results)
                                  (z qh-results) (w qh-results))
                 final-norm-val)))))))))

(defun-g hermite-noise-unnormalized-gradients ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx0 hash-grady0 hash-gradz0
                                      hash-gradx1 hash-grady1 hash-gradz1)
        (bs-fast32-hash-3-per-corner pi)
      (setf hash-gradx0 (- hash-gradx0 (v4! 0.49999)))
      (setf hash-grady0 (- hash-grady0 (v4! 0.49999)))
      (setf hash-gradz0 (- hash-gradz0 (v4! 0.49999)))
      (setf hash-gradx1 (- hash-gradx1 (v4! 0.49999)))
      (setf hash-grady1 (- hash-grady1 (v4! 0.49999)))
      (setf hash-gradz1 (- hash-gradz1 (v4! 0.49999)))
      (let* ((final-norm-val (/ 1.0 0.46875)))
        (multiple-value-bind (ival-results igrad-results-x igrad-results-y)
            (quintic-hermite
             (z pf)
             hash-gradx0 hash-gradx1 hash-grady0
             hash-grady1 hash-gradz0 hash-gradz1)
          (let* ((qh-results
                  (quintic-hermite
                   (y pf)
                   (v! (s~ ival-results :xy) (s~ igrad-results-x :xy))
                   (v! (s~ ival-results :zw) (s~ igrad-results-x :zw))
                   (v! (s~ igrad-results-y :xy) 0.0 0.0)
                   (v! (s~ igrad-results-y :zw) 0.0 0.0))))
            (* (quintic-hermite (x pf) (x qh-results) (y qh-results) (z qh-results)
                                (w qh-results))
               final-norm-val)))))))

;;------------------------------------------------------------
