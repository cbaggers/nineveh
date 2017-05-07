(in-package :nineveh.noise)

;;------------------------------------------------------------
;; 2D

;; Perlin Noise 2D  ( gradient noise )
;; Return value range of -1.0->1.0

(defun-g perlin-noise ((p :vec2))
  ;; looks much better than revised noise in 2D, and with an efficent hash
  ;; function runs at about the same speed.
  ;;
  ;; requires 2 random numbers per point.
  (let* ((pi (floor p))
         (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0))))))
    (multiple-value-bind (hash-x hash-y) (bs-fast32-hash-2-per-corner pi)
      (let* ((grad-x (- hash-x (v4! 0.49999)))
             (grad-y (- hash-y (v4! 0.49999)))
             (grad-results
              (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                 (+ (* grad-x (s~ pf-pfmin1 :xzxz))
                    (* grad-y (s~ pf-pfmin1 :yyww))))))
        (multf grad-results (v4! 1.4142135))
        (let* ((blend (perlin-quintic (s~ pf-pfmin1 :xy)))
               (blend2 (v! blend (- (v2! 1.0) blend))))
          (dot grad-results (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))))

;; (defun-g perlin-noise ((hash-func (function (:vec2) (:vec4 :vec4)))
;;                            (p :vec2))
;;   ;; looks much better than revised noise in 2D, and with an efficent hash
;;   ;; function runs at about the same speed.
;;   ;;
;;   ;; requires 2 random numbers per point.
;;   (let* ((pi (floor p))
;;          (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0))))))
;;     (multiple-value-bind (hash-x hash-y) (funcall hash-func pi)
;;       (let* ((grad-x (- hash-x (v4! 0.49999)))
;;              (grad-y (- hash-y (v4! 0.49999)))
;;              (grad-results
;;               (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
;;                  (+ (* grad-x (s~ pf-pfmin1 :xzxz))
;;                     (* grad-y (s~ pf-pfmin1 :yyww))))))
;;         (multf grad-results (v4! 1.4142135))
;;         (let* ((blend (perlin-quintic (s~ pf-pfmin1 :xy)))
;;                (blend2 (v! blend (- (v2! 1.0) blend))))
;;           (dot grad-results (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))))

(defun-g perlin-noise-surflet ((p :vec2))
  ;; Classic Perlin Surflet
  ;; http://briansharpe.wordpress.com/2012/03/09/modifications-to-classic-perlin-noise/
  (let* ((pi (floor p))
         (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0))))))
    (multiple-value-bind (hash-x hash-y) (bs-fast32-hash-2-per-corner pi)
      (let* ((grad-x (- hash-x (v4! 0.49999)))
             (grad-y (- hash-y (v4! 0.49999)))
             (grad-results
              (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                 (+ (* grad-x (s~ pf-pfmin1 :xzxz))
                    (* grad-y (s~ pf-pfmin1 :yyww))))))
        (multf grad-results (v4! 2.3703704))
        (let* ((vecs-len-sq (* pf-pfmin1 pf-pfmin1)))
          (setf vecs-len-sq (+ (s~ vecs-len-sq :xzxz) (s~ vecs-len-sq :yyww)))
          (dot (falloff-xsq-c2 (min (v4! 1.0) vecs-len-sq)) grad-results))))))

(defun-g perlin-noise-revised ((p :vec2))
  ;; 2D improved perlin noise.
  ;; requires 1 random value per point.
  ;; does not look as good as classic in 2D due to only a small number of
  ;; possible cell types.  But can run a lot faster than classic perlin noise
  ;; if the hash function is slow
  (let* ((pi (floor p))
         (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0)))))
         (hash (bs-fast32-hash pi)))
    (decf hash (v4! 0.5))
    (let* ((grad-results
            (+ (* (s~ pf-pfmin1 :xzxz) (sign hash))
               (* (s~ pf-pfmin1 :yyww) (sign (- (abs hash) (v4! 0.25))))))
           (blend (perlin-quintic (s~ pf-pfmin1 :xy)))
           (blend2 (v! blend (- (v2! 1.0) blend))))
      (dot grad-results (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))

;; (defun-g perlin-noise-revised ((hash-func (function (:vec2) :vec4))
;;                             (p :vec2))
;;   ;; 2D 'improved' perlin noise.
;;   ;; requires 1 random value per point.
;;   ;; does not look as good as classic in 2D due to only a small number of
;;   ;; possible cell types.  But can run a lot faster than classic perlin noise
;;   ;; if the hash function is slow
;;   (let* ((pi (floor p))
;;          (pf-pfmin1 (- (s~ p :xyxy) (v! pi (+ pi (v2! 1.0)))))
;;          (hash (funcall hash-func pi)))
;;     (decf hash (v4! 0.5))
;;     (let* ((grad-results
;;             (+ (* (s~ pf-pfmin1 :xzxz) (sign hash))
;;                (* (s~ pf-pfmin1 :yyww) (sign (- (abs hash) (v4! 0.25))))))
;;            (blend (perlin-quintic (s~ pf-pfmin1 :xy)))
;;            (blend2 (v! blend (- (v2! 1.0) blend))))
;;       (dot grad-results (* (s~ blend2 :zxzx) (s~ blend2 :wwyy))))))

(defun-g perlin-noise-simplex ((p :vec2))
  (let* ((skew-factor 0.36602542)
         (unskew-factor 0.21132489)
         (simplex-tri-height 0.7071068)
         (simplex-points
          (v3! (- 1.0 unskew-factor) (- unskew-factor)
               (- 1.0 (* 2.0 unskew-factor)))))
    (multf p (v2! simplex-tri-height))
    (let* ((pi (floor (+ p (v2! (dot p (v2! skew-factor)))))))
      (multiple-value-bind (hash-x hash-y)
          (bs-fast32-hash-2-per-corner pi)
        (let* ((v0 (- pi (- (v2! (dot pi (v2! unskew-factor))) p)))
               (v1pos-v1hash
                (if (< (x v0) (y v0))
                    (v! (s~ simplex-points :xy) (y hash-x) (y hash-y))
                    (v! (s~ simplex-points :yx) (z hash-x) (z hash-y))))
               (v12
                (+ (v! (s~ v1pos-v1hash :xy) (s~ simplex-points :zz))
                   (s~ v0 :xyxy)))
               (grad-x (- (v3! (x hash-x) (z v1pos-v1hash) (w hash-x))
                          (v3! 0.49999)))
               (grad-y (- (v3! (x hash-y) (w v1pos-v1hash) (w hash-y))
                          (v3! 0.49999)))
               (grad-results
                (* (inversesqrt (+ (* grad-x grad-x) (* grad-y grad-y)))
                   (+ (* grad-x (v! (x v0) (s~ v12 :xz)))
                      (* grad-y (v! (y v0) (s~ v12 :yw))))))
               (final-normalization 99.20434)
               (m
                (+ (* (v! (x v0) (s~ v12 :xz))
                      (v! (x v0) (s~ v12 :xz)))
                   (* (v! (y v0) (s~ v12 :yw))
                      (v! (y v0) (s~ v12 :yw))))))
          (setf m (max (- (v3! 0.5) m) 0.0))
          (setf m (* m m))
          (* (dot (* m m) grad-results) final-normalization))))))

;;------------------------------------------------------------
;; 3D

(defun-g perlin-noise ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi))
         (pf-min1 (- pf (v3! 1.0))))
    (multiple-value-bind (hashx0 hashy0 hashz0 hashx1 hashy1 hashz1)
        (bs-fast32-hash-3-per-corner pi)
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
                     (* (s~ pf-min1 :zzzz) grad-z1)))))
             (blend (perlin-quintic pf))
             (res0 (mix grad-results-0 grad-results-1 (z blend)))
             (blend2 (v! (s~ blend :xy) (- (v2! 1.0) (s~ blend :xy))))
             (final (dot res0 (* (s~ blend2 :zxzx) (s~ blend2 :wwyy)))))
        (multf final 1.1547005)
        final))))

(defun-g perlin-noise-simplex ((p :vec3))
  (multiple-value-bind (pi pi-1 pi-2 v1234-x v1234-y v1234-z)
      (simplex-3d-get-corner-vectors p)
    (multiple-value-bind (hash-0 hash-1 hash-2)
        (bs-fast32-hash-3-per-corner pi pi-1 pi-2)
      (decf hash-0 (v4! 0.49999))
      (decf hash-1 (v4! 0.49999))
      (decf hash-2 (v4! 0.49999))
      (let* ((grad-results
              (*
               (inversesqrt
                (+ (* hash-0 hash-0) (+ (* hash-1 hash-1) (* hash-2 hash-2))))
               (+ (* hash-0 v1234-x)
                  (+ (* hash-1 v1234-y) (* hash-2 v1234-z)))))
             (final-normalization 37.837227))
        (* (dot (simplex-3d-get-surflet-weights v1234-x v1234-y v1234-z)
                grad-results)
         final-normalization)))))

(defun-g perlin-noise-surflet ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi))
         (pf-min1 (- pf (v3! 1.0)))
         ((hashx0 :vec4))
         ((hashy0 :vec4))
         ((hashz0 :vec4))
         ((hashx1 :vec4))
         ((hashy1 :vec4))
         ((hashz1 :vec4)))
    (multiple-value-bind (hashx0 hashy0 hashz0 hashx1 hashy1 hashz1)
        (bs-fast32-hash-3-per-corner pi)
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
        (multf pf pf)
        (multf pf-min1 pf-min1)
        (let* ((vecs-len-sq
                (+ (v4! (x pf) (x pf-min1) (x pf) (x pf-min1))
                   (v! (s~ pf :yy) (s~ pf-min1 :yy))))
               (final
                (+
                 (dot
                  (falloff-xsq-c2
                   (min (v4! 1.0) (+ vecs-len-sq (s~ pf :zzzz))))
                  grad-results-0)
                 (dot
                  (falloff-xsq-c2
                   (min (v4! 1.0) (+ vecs-len-sq (s~ pf-min1 :zzzz))))
                  grad-results-1))))
          (multf final 2.3703704)
          final)))))

(defun-g perlin-noise-revised ((p :vec3))
  (let* ((pi (floor p))
         (pf (- p pi))
         (pf-min1 (- pf (v3! 1.0))))
    (multiple-value-bind (hash-lowz hash-highz) (bs-fast32-hash pi)
      (decf hash-lowz (v4! 0.5))
      (let* ((grad-results-0-0
              (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) (sign hash-lowz))))
        (setf hash-lowz (- (abs hash-lowz) (v4! 0.25)))
        (let* ((grad-results-0-1
                (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) (sign hash-lowz)))
               (grad-results-0-2
                (* (s~ pf :zzzz) (sign (- (abs hash-lowz) (v4! 0.125)))))
               (grad-results-0
                (+ grad-results-0-0 (+ grad-results-0-1 grad-results-0-2))))
          (decf hash-highz (v4! 0.5))
          (let* ((grad-results-1-0
                  (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) (sign hash-highz))))
            (setf hash-highz (- (abs hash-highz) (v4! 0.25)))
            (let* ((grad-results-1-1
                    (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) (sign hash-highz)))
                   (grad-results-1-2
                    (* (s~ pf-min1 :zzzz) (sign (- (abs hash-highz)
                                                   (v4! 0.125)))))
                   (grad-results-1
                    (+ grad-results-1-0 (+ grad-results-1-1 grad-results-1-2)))
                   (blend (perlin-quintic pf))
                   (res0 (mix grad-results-0 grad-results-1 (z blend)))
                   (blend2 (v! (s~ blend :xy) (- (v2! 1.0) (s~ blend :xy)))))
              (* (dot res0 (* (s~ blend2 :zxzx) (s~ blend2 :wwyy)))
                 (/ 2.0 3.0)))))))))

;;------------------------------------------------------------
;; 4D

(defun-g perlin-noise ((p :vec4))
  (let* ((pi (floor p))
         (pf (- p pi))
         (pf-min1 (- pf (v4! 1.0)))
         ((lowz-loww-hash-0 :vec4))
         ((lowz-loww-hash-1 :vec4))
         ((lowz-loww-hash-2 :vec4))
         ((lowz-loww-hash-3 :vec4))
         ((highz-loww-hash-0 :vec4))
         ((highz-loww-hash-1 :vec4))
         ((highz-loww-hash-2 :vec4))
         ((highz-loww-hash-3 :vec4))
         ((lowz-highw-hash-0 :vec4))
         ((lowz-highw-hash-1 :vec4))
         ((lowz-highw-hash-2 :vec4))
         ((lowz-highw-hash-3 :vec4))
         ((highz-highw-hash-0 :vec4))
         ((highz-highw-hash-1 :vec4))
         ((highz-highw-hash-2 :vec4))
         ((highz-highw-hash-3 :vec4)))
    (multiple-value-bind (lowz-loww-hash-0
                          lowz-loww-hash-1
                          lowz-loww-hash-2
                          lowz-loww-hash-3
                          highz-loww-hash-0
                          highz-loww-hash-1
                          highz-loww-hash-2
                          highz-loww-hash-3
                          lowz-highw-hash-0
                          lowz-highw-hash-1
                          lowz-highw-hash-2
                          lowz-highw-hash-3
                          highz-highw-hash-0
                          highz-highw-hash-1
                          highz-highw-hash-2
                          highz-highw-hash-3)
        (bs-quick32-hash-4-per-corner pi))
    (decf lowz-loww-hash-0 (v4! 0.49999))
    (decf lowz-loww-hash-1 (v4! 0.49999))
    (decf lowz-loww-hash-2 (v4! 0.49999))
    (decf lowz-loww-hash-3 (v4! 0.49999))
    (decf highz-loww-hash-0 (v4! 0.49999))
    (decf highz-loww-hash-1 (v4! 0.49999))
    (decf highz-loww-hash-2 (v4! 0.49999))
    (decf highz-loww-hash-3 (v4! 0.49999))
    (decf lowz-highw-hash-0 (v4! 0.49999))
    (decf lowz-highw-hash-1 (v4! 0.49999))
    (decf lowz-highw-hash-2 (v4! 0.49999))
    (decf lowz-highw-hash-3 (v4! 0.49999))
    (decf highz-highw-hash-0 (v4! 0.49999))
    (decf highz-highw-hash-1 (v4! 0.49999))
    (decf highz-highw-hash-2 (v4! 0.49999))
    (decf highz-highw-hash-3 (v4! 0.49999))
    (let* ((grad-results-lowz-loww
            (inversesqrt
             (+ (* lowz-loww-hash-0 lowz-loww-hash-0)
                (+ (* lowz-loww-hash-1 lowz-loww-hash-1)
                   (+ (* lowz-loww-hash-2 lowz-loww-hash-2)
                      (* lowz-loww-hash-3 lowz-loww-hash-3)))))))
      (multf grad-results-lowz-loww
             (+ (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) lowz-loww-hash-0)
                (+ (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) lowz-loww-hash-1)
                   (+ (* (s~ pf :zzzz) lowz-loww-hash-2)
                      (* (s~ pf :wwww) lowz-loww-hash-3)))))
      (let* ((grad-results-highz-loww
              (inversesqrt
               (+ (* highz-loww-hash-0 highz-loww-hash-0)
                  (+ (* highz-loww-hash-1 highz-loww-hash-1)
                     (+ (* highz-loww-hash-2 highz-loww-hash-2)
                        (* highz-loww-hash-3 highz-loww-hash-3)))))))
        (multf grad-results-highz-loww
               (+ (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) highz-loww-hash-0)
                  (+ (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) highz-loww-hash-1)
                     (+ (* (s~ pf-min1 :zzzz) highz-loww-hash-2)
                        (* (s~ pf :wwww) highz-loww-hash-3)))))
        (let* ((grad-results-lowz-highw
                (inversesqrt
                 (+ (* lowz-highw-hash-0 lowz-highw-hash-0)
                    (+ (* lowz-highw-hash-1 lowz-highw-hash-1)
                       (+ (* lowz-highw-hash-2 lowz-highw-hash-2)
                          (* lowz-highw-hash-3 lowz-highw-hash-3)))))))
          (multf grad-results-lowz-highw
                 (+ (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) lowz-highw-hash-0)
                    (+ (* (s~ (v2! (y pf) (y pf-min1)) :xxyy) lowz-highw-hash-1)
                       (+ (* (s~ pf :zzzz) lowz-highw-hash-2)
                          (* (s~ pf-min1 :wwww) lowz-highw-hash-3)))))
          (let* ((grad-results-highz-highw
                  (inversesqrt
                   (+ (* highz-highw-hash-0 highz-highw-hash-0)
                      (+ (* highz-highw-hash-1 highz-highw-hash-1)
                         (+ (* highz-highw-hash-2 highz-highw-hash-2)
                            (* highz-highw-hash-3 highz-highw-hash-3)))))))
            (multf grad-results-highz-highw
                   (+ (* (s~ (v2! (x pf) (x pf-min1)) :xyxy) highz-highw-hash-0)
                      (+
                       (* (s~ (v2! (y pf) (y pf-min1)) :xxyy)
                          highz-highw-hash-1)
                       (+ (* (s~ pf-min1 :zzzz) highz-highw-hash-2)
                          (* (s~ pf-min1 :wwww) highz-highw-hash-3)))))
            (let* ((blend (perlin-quintic pf))
                   (res0
                    (+ grad-results-lowz-loww
                       (*
                        (- grad-results-lowz-highw grad-results-lowz-loww)
                        (s~ blend :wwww))))
                   (res1
                    (+ grad-results-highz-loww
                       (*
                        (- grad-results-highz-highw
                           grad-results-highz-loww)
                        (s~ blend :wwww)))))
              (setf res0 (+ res0 (* (- res1 res0) (s~ blend :zzzz))))
              (setf (s~ blend :zw) (- (v2! 1.0) (s~ blend :xy)))
              (dot res0 (* (s~ blend :zxzx) (s~ blend :wwyy))))))))))
