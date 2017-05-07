(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

(defun-g hermite-noise ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx hash-grady)
        (bs-fast32-hash-2-per-corner pi)
      (setf hash-gradx (- hash-gradx (v4! "0.49999")))
      (setf hash-grady (- hash-grady (v4! "0.49999")))
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

(defun-g hermite-noise-deriv ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx hash-grady)
        (bs-fast32-hash-2-per-corner pi)
      (setf hash-gradx (- hash-gradx (v4! "0.49999")))
      (setf hash-grady (- hash-grady (v4! "0.49999")))
      (let* ((norm
              (inversesqrt
               (+ (* hash-gradx hash-gradx) (* hash-grady hash-grady)))))
        (multf hash-gradx norm)
        (multf hash-grady norm)
        (let* ((final-norm-val 2.2627418)
               (qh-results-x
                (quintic-hermite (y pf) (s~ hash-gradx :xy) (s~ hash-gradx :zw)
                                 (s~ hash-grady :xy) (s~ hash-grady :zw)))
               (qh-results-y
                (quintic-hermite (x pf) (s~ hash-grady :xz) (s~ hash-grady :yw)
                                 (s~ hash-gradx :xz) (s~ hash-gradx :yw)))
               (finalpos
                (quintic-hermite (x pf) (x qh-results-x) (y qh-results-x)
                                 (z qh-results-x) (w qh-results-x)))
               (deriv-x
                (quintic-hermite-deriv (x pf) (x qh-results-x) (y qh-results-x)
                                       (z qh-results-x) (w qh-results-x)))
               (deriv-y
                (quintic-hermite-deriv (y pf) (x qh-results-y) (y qh-results-y)
                                       (z qh-results-y) (w qh-results-y))))
          (* (v3! finalpos deriv-x deriv-y) final-norm-val))))))

(defun-g hermite-noise-unnormalized-gradients ((p :vec2))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx hash-grady)
        (bs-fast32-hash-2-per-corner pi)
      (setf hash-gradx (- hash-gradx (v4! "0.49999")))
      (setf hash-grady (- hash-grady (v4! "0.49999")))
      (let* ((final-norm-val 3.2)
             (qh-results
              (quintic-hermite (y pf) (s~ hash-gradx :xy) (s~ hash-gradx :zw)
                               (s~ hash-grady :xy) (s~ hash-grady :zw))))
        (*
         (quintic-hermite (x pf) (x qh-results) (y qh-results) (z qh-results)
                          (w qh-results))
         final-norm-val)))))

(defun-g hermite-noise-unnormalized-gradients-deriv ((p :vec2))
  (let* ((pi (floor p)) (pf (- p pi)) ((hash-gradx :vec4)) ((hash-grady :vec4)))
    (multiple-value-bind (hash-gradx hash-grady)
        (bs-fast32-hash-2-per-corner pi)
      (setf hash-gradx (- hash-gradx (v4! "0.49999")))
      (setf hash-grady (- hash-grady (v4! "0.49999")))
      (let* ((final-norm-val 3.2)
             (qh-results-x
              (quintic-hermite (y pf) (s~ hash-gradx :xy) (s~ hash-gradx :zw)
                               (s~ hash-grady :xy) (s~ hash-grady :zw)))
             (qh-results-y
              (quintic-hermite (x pf) (s~ hash-grady :xz) (s~ hash-grady :yw)
                               (s~ hash-gradx :xz) (s~ hash-gradx :yw)))
             (finalpos
              (quintic-hermite (x pf) (x qh-results-x) (y qh-results-x)
                               (z qh-results-x) (w qh-results-x)))
             (deriv-x
              (quintic-hermite-deriv (x pf) (x qh-results-x) (y qh-results-x)
                                     (z qh-results-x) (w qh-results-x)))
             (deriv-y
              (quintic-hermite-deriv (y pf) (x qh-results-y) (y qh-results-y)
                                     (z qh-results-y) (w qh-results-y))))
        (* (v3! finalpos deriv-x deriv-y) final-norm-val)))))

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
      (setf hash-gradx0 (- hash-gradx0 (v4! "0.49999")))
      (setf hash-grady0 (- hash-grady0 (v4! "0.49999")))
      (setf hash-gradz0 (- hash-gradz0 (v4! "0.49999")))
      (setf hash-gradx1 (- hash-gradx1 (v4! "0.49999")))
      (setf hash-grady1 (- hash-grady1 (v4! "0.49999")))
      (setf hash-gradz1 (- hash-gradz1 (v4! "0.49999")))
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

(defun-g hermite-noise-deriv ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx0 hash-grady0 hash-gradz0
                                      hash-gradx1 hash-grady1 hash-gradz1)
        (bs-fast32-hash-3-per-corner pi)
      (setf hash-gradx0 (- hash-gradx0 (v4! "0.49999")))
      (setf hash-grady0 (- hash-grady0 (v4! "0.49999")))
      (setf hash-gradz0 (- hash-gradz0 (v4! "0.49999")))
      (setf hash-gradx1 (- hash-gradx1 (v4! "0.49999")))
      (setf hash-grady1 (- hash-grady1 (v4! "0.49999")))
      (setf hash-gradz1 (- hash-gradz1 (v4! "0.49999")))
      (let* ((norm0
              (inversesqrt
               (+ (* hash-gradx0 hash-gradx0)
                  (+ (* hash-grady0 hash-grady0) (* hash-gradz0 hash-gradz0))))))
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
            (multiple-value-bind (ival-results-z igrad-results-x-z igrad-results-y-z)
                (quintic-hermite (z pf)
                                 hash-gradx0 hash-gradx1 hash-grady0
                                 hash-grady1 hash-gradz0 hash-gradz1)
              (multiple-value-bind (ival-results-y
                                    igrad-results-x-y
                                    igrad-results-z-y)
                  (quintic-hermite
                   (y pf)
                   (v! (s~ hash-gradx0 :xy) (s~ hash-gradx1 :xy))
                   (v! (s~ hash-gradx0 :zw) (s~ hash-gradx1 :zw))
                   (v! (s~ hash-gradz0 :xy) (s~ hash-gradz1 :xy))
                   (v! (s~ hash-gradz0 :zw) (s~ hash-gradz1 :zw))
                   (v! (s~ hash-grady0 :xy) (s~ hash-grady1 :xy))
                   (v! (s~ hash-grady0 :zw) (s~ hash-grady1 :zw)))
                (let* ((qh-results-x
                        (quintic-hermite
                         (y pf)
                         (v! (s~ ival-results-z :xy) (s~ igrad-results-x-z :xy))
                         (v! (s~ ival-results-z :zw) (s~ igrad-results-x-z :zw))
                         (v! (s~ igrad-results-y-z :xy) 0.0 0.0)
                         (v! (s~ igrad-results-y-z :zw) 0.0 0.0)))
                       (qh-results-y
                        (quintic-hermite
                         (x pf)
                         (v! (s~ ival-results-z :xz) (s~ igrad-results-y-z :xz))
                         (v! (s~ ival-results-z :yw) (s~ igrad-results-y-z :yw))
                         (v! (s~ igrad-results-x-z :xz) 0.0 0.0)
                         (v! (s~ igrad-results-x-z :yw) 0.0 0.0)))
                       (qh-results-z
                        (quintic-hermite
                         (x pf)
                         (v! (s~ ival-results-y :xz) (s~ igrad-results-z-y :xz))
                         (v! (s~ ival-results-y :yw) (s~ igrad-results-z-y :yw))
                         (v! (s~ igrad-results-x-y :xz) 0.0 0.0)
                         (v! (s~ igrad-results-x-y :yw) 0.0 0.0)))
                       (deriv-x
                        (quintic-hermite-deriv (x pf) (x qh-results-x)
                                               (y qh-results-x) (z qh-results-x) (w qh-results-x)))
                       (deriv-y
                        (quintic-hermite-deriv (y pf) (x qh-results-y)
                                               (y qh-results-y) (z qh-results-y) (w qh-results-y)))
                       (deriv-z
                        (quintic-hermite-deriv (z pf) (x qh-results-z)
                                               (y qh-results-z) (z qh-results-z) (w qh-results-z)))
                       (finalpos
                        (quintic-hermite (x pf) (x qh-results-x) (y qh-results-x)
                                         (z qh-results-x) (w qh-results-x))))
                  (* (v4! finalpos deriv-x deriv-y deriv-z)
                     final-norm-val))))))))))

(defun-g hermite-noise-unnormalized-gradients ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx0 hash-grady0 hash-gradz0
                                      hash-gradx1 hash-grady1 hash-gradz1)
        (bs-fast32-hash-3-per-corner pi)
      (setf hash-gradx0 (- hash-gradx0 (v4! "0.49999")))
      (setf hash-grady0 (- hash-grady0 (v4! "0.49999")))
      (setf hash-gradz0 (- hash-gradz0 (v4! "0.49999")))
      (setf hash-gradx1 (- hash-gradx1 (v4! "0.49999")))
      (setf hash-grady1 (- hash-grady1 (v4! "0.49999")))
      (setf hash-gradz1 (- hash-gradz1 (v4! "0.49999")))
      (let* ((final-norm-val (/ 1.0 0.46875)))
        (multiple-value-bind (ival-results igrad-results-x igrad-results-y)
            (quintic-hermite
             (z pf)
             hash-gradx0 hash-gradx1
             hash-grady0 hash-grady1
             hash-gradz0 hash-gradz1)
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

(defun-g hermite-noise-unnormalized-gradients-deriv ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi)))
    (multiple-value-bind (hash-gradx0 hash-grady0 hash-gradz0 hash-gradx1
                                      hash-grady1 hash-gradz1)
        (bs-fast32-hash-3-per-corner pi)
      (setf hash-gradx0 (- hash-gradx0 (v4! "0.49999")))
      (setf hash-grady0 (- hash-grady0 (v4! "0.49999")))
      (setf hash-gradz0 (- hash-gradz0 (v4! "0.49999")))
      (setf hash-gradx1 (- hash-gradx1 (v4! "0.49999")))
      (setf hash-grady1 (- hash-grady1 (v4! "0.49999")))
      (setf hash-gradz1 (- hash-gradz1 (v4! "0.49999")))
      (let* ((final-norm-val (/ 1.0 0.46875)))
        (multiple-value-bind (ival-results-z
                              igrad-results-x-z
                              igrad-results-y-z)
            (quintic-hermite (z pf)
                             hash-gradx0 hash-gradx1
                             hash-grady0 hash-grady1
                             hash-gradz0 hash-gradz1)
          (multiple-value-bind (ival-results-y
                                igrad-results-x-y
                                igrad-results-z-y)
              (quintic-hermite (y pf)
                               (v! (s~ hash-gradx0 :xy) (s~ hash-gradx1 :xy))
                               (v! (s~ hash-gradx0 :zw) (s~ hash-gradx1 :zw))
                               (v! (s~ hash-gradz0 :xy) (s~ hash-gradz1 :xy))
                               (v! (s~ hash-gradz0 :zw) (s~ hash-gradz1 :zw))
                               (v! (s~ hash-grady0 :xy) (s~ hash-grady1 :xy))
                               (v! (s~ hash-grady0 :zw) (s~ hash-grady1 :zw)))
            (let* ((qh-results-x
                    (quintic-hermite
                     (y pf)
                     (v! (s~ ival-results-z :xy) (s~ igrad-results-x-z :xy))
                     (v! (s~ ival-results-z :zw) (s~ igrad-results-x-z :zw))
                     (v! (s~ igrad-results-y-z :xy) 0.0 0.0)
                     (v! (s~ igrad-results-y-z :zw) 0.0 0.0)))
                   (qh-results-y
                    (quintic-hermite
                     (x pf)
                     (v! (s~ ival-results-z :xz) (s~ igrad-results-y-z :xz))
                     (v! (s~ ival-results-z :yw) (s~ igrad-results-y-z :yw))
                     (v! (s~ igrad-results-x-z :xz) 0.0 0.0)
                     (v! (s~ igrad-results-x-z :yw) 0.0 0.0)))
                   (qh-results-z
                    (quintic-hermite
                     (x pf)
                     (v! (s~ ival-results-y :xz) (s~ igrad-results-z-y :xz))
                     (v! (s~ ival-results-y :yw) (s~ igrad-results-z-y :yw))
                     (v! (s~ igrad-results-x-y :xz) 0.0 0.0)
                     (v! (s~ igrad-results-x-y :yw) 0.0 0.0)))
                   (deriv-x
                    (quintic-hermite-deriv (x pf) (x qh-results-x)
                                           (y qh-results-x) (z qh-results-x) (w qh-results-x)))
                   (deriv-y
                    (quintic-hermite-deriv (y pf) (x qh-results-y)
                                           (y qh-results-y) (z qh-results-y) (w qh-results-y)))
                   (deriv-z
                    (quintic-hermite-deriv (z pf) (x qh-results-z)
                                           (y qh-results-z) (z qh-results-z) (w qh-results-z)))
                   (finalpos
                    (quintic-hermite (x pf) (x qh-results-x) (y qh-results-x)
                                     (z qh-results-x) (w qh-results-x))))
              (* (v4! finalpos deriv-x deriv-y deriv-z) final-norm-val))))))))

;;------------------------------------------------------------
