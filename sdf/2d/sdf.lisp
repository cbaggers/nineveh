(in-package :nineveh.sdf.2d)

;;============================================================
;; Big love to:
;; - Maarten: for the fantastic 2D sdf function playground
;;;  which is the principle basis for this code:
;;   https://www.shadertoy.com/view/4dfXDn
;; - iq: for many example sdf functions on shadertoy
;; - iq: for many example sdf functions on shadertoy
;;============================================================

;;------------------------------------------------------------
;; Combine distance field functions

(defun-g merge-smooth ((d1 :float) (d2 :float) (k :float))
  (let ((h (clamp (+ 0.5
                     (* 0.5
                        (/ (- d2 d1)
                           k)))
                  0.0
                  1.0)))
    (- (mix d2 d1 h)
       (* k h (- 1.0 h)))))

(defun-g merge-simple ((d1 :float) (d2 :float))
  (min d1 d2))

(defun-g merge-exclude ((d1 :float) (d2 :float))
  (min (max (- d1) d2)
       (max (- d2) d1)))

(defun-g subtract ((d1 :float) (d2 :float))
  (max (- d1) d2))

(defun-g intersect ((d1 :float) (d2 :float))
  (max d1 d2))

;;------------------------------------------------------------
;; Masks for drawing

(defun-g mask-fill ((dist :float))
  (clamp (- dist) 0.0 1.0))

(defun-g mask-border-inner ((dist :float) (width :float))
  (let* ((alpha1 (clamp (+ dist width) 0.0 1.0))
         (alpha2 (clamp dist 0.0 1.0)))
    (- alpha1 alpha2)))

(defun-g mask-border-outer ((dist :float) (width :float))
  (let* ((alpha1 (clamp dist 0.0 1.0))
         (alpha2 (clamp (- dist width) 0.0 1.0)))
    (- alpha1 alpha2)))

;;------------------------------------------------------------
;; Light & Shadow

(defun-g cast-shadow ((fn (function (:vec2) :float))
                      (p :vec2)
                      (light-position :vec2)
                      (source-radius :float))
  (let* ((dir (normalize (- light-position p)))
         ;; distance to light
         (dl (length (- p light-position)))
         ;; fraction of light visible, starts at one radius
         ;; (second half added in the end)
         (lf (* source-radius dl))
         ;; distance traveled
         (dt 0.01))
    (dotimes (i 64)
      (let (;; distance to scene at current position
            (sd (funcall fn (+ p (* dir dt)))))
        ;; early out when this ray is guaranteed to be full shadow
        (when (< sd (- source-radius))
          (return 0.0))
        ;; width of cone-overlap at light
        ;; 0 in center, so 50% overlap: add one radius outside of loop to
        ;; get total coverage.
        ;; should be '(sd / dt) * dl', but '*dl' outside of loop
        (setf lf (min lf (/ sd dt)))
        ;; move ahead
        (incf dt (max 1.0 (abs sd)))
        (when (> dt dl)
          (break))))
    ;; multiply by dl to get the real projected overlap (moved out of loop)
    ;; add one radius, before between -radius and + radius
    ;; normalize to 1 ( / 2*radius)
    (setf lf (clamp (/ (+ (* lf dl) source-radius) (* 2.0 source-radius))
                    0.0
                    1.0))
    (setf lf (smoothstep 0.0 1.0 lf))
    lf))

;; Not exposing the shaped versions yet, I want to work out a nicer term
;; for the shaping function

(defun-g shaped-light-with-source ((fn (function (:vec2) :float))
                                   (light-fn (function (:vec2) :float))
                                   (p :vec2)
                                   (light-position :vec2)
                                   (light-color :vec4)
                                   (light-range :float)
                                   (source-radius :float))
  (let* (;; distance to light
         (ld (funcall light-fn (- p light-position))))
    (if (> ld light-range)
        (vec4 0.0)
        (let* ((shad (cast-shadow fn p light-position source-radius))
               (fall (/ (- light-range ld) light-range))
               (fall (* fall fall))
               (source (mask-fill (- ld source-radius))))
          (* (+ (* shad fall) source)
             light-color)))))

(defun-g shaped-light ((fn (function (:vec2) :float))
                       (light-fn (function (:vec2) :float))
                       (p :vec2)
                       (light-position :vec2)
                       (light-color :vec4)
                       (light-range :float)
                       (source-radius :float))
  (let* (;; distance to light
         (ld (funcall light-fn (- p light-position))))
    (if (> ld light-range)
        (vec4 0.0)
        (let* ((shad (cast-shadow fn p light-position source-radius))
               (fall (/ (- light-range ld) light-range))
               (fall (* fall fall)))
          (* shad fall light-color)))))

(defun-g point-light-with-source ((fn (function (:vec2) :float))
                                  (p :vec2)
                                  (light-position :vec2)
                                  (light-color :vec4)
                                  (light-range :float)
                                  (source-radius :float))
  (shaped-light-with-source fn
                            #'(length :vec2)
                            p
                            light-position
                            light-color
                            light-range
                            source-radius))

(defun-g point-light ((fn (function (:vec2) :float))
                      (p :vec2)
                      (light-position :vec2)
                      (light-color :vec4)
                      (light-range :float)
                      (source-radius :float))
  (shaped-light fn
                #'(length :vec2)
                p
                light-position
                light-color
                light-range
                source-radius))


;;------------------------------------------------------------
;; Distance Field Functions

(defun-g rhombus ((p :vec2)
                  (size :vec2)
                  (corner-radius :float))
  (flet ((ndot ((a :vec2) (b :vec2))
           (- (* (x a) (x b))
              (* (y a) (y b)))))
    (let* ((q (abs p))
           (h (clamp (/ (+ (* -2.0 (ndot q size))
                           (ndot size size))
                        (dot size size))
                     -1.0
                     1.0))
           (d (length (- q (* 0.5 size (vec2 (- 1.0 h) (+ 1.0 h))))))
           (d (* d (sign (- (+ (* (x q) (y size))
                               (* (y q) (x size)))
                            (* (x size) (y size)))))))
      (- d corner-radius))))

(defun-g circle ((p :vec2) (radius :float))
  (- (length p) radius))

(defun-g triangle ((p :vec2) (radius :float))
  (- (max (+ (* (x (abs p)) 0.866025)
             (* (y p) 0.5))
          (- (y p)))
     (* 0.5 radius)))

(defun-g triangle ((p :vec2)
                   (corner0 :vec2)
                   (corner1 :vec2)
                   (corner2 :vec2))
  (let* ((e0 (- corner1 corner0))
         (e1 (- corner2 corner1))
         (e2 (- corner0 corner2))
         (v0 (- p corner0))
         (v1 (- p corner1))
         (v2 (- p corner2))
         (pq0 (- v0 (* e0 (clamp (/ (dot v0 e0) (dot e0 e0))
                                 0.0
                                 1.0))))
         (pq1 (- v1 (* e1 (clamp (/ (dot v1 e1) (dot e1 e1))
                                 0.0
                                 1.0))))
         (pq2 (- v2 (* e2 (clamp (/ (dot v2 e2) (dot e2 e2))
                                 0.0
                                 1.0))))
         (s (sign (- (* (x e0) (y e2))
                     (* (y e0) (x e2)))))
         (d (min (min (v2! (dot pq0 pq0)
                           (* s (- (* (x v0) (y e0))
                                   (* (y v0) (x e0)))))
                      (v2! (dot pq1 pq1)
                           (* s (- (* (x v1) (y e1))
                                   (* (y v1) (x e1))))))
                 (v2! (dot pq2 pq2) (* s (- (* (x v2) (y e2))
                                            (* (y v2) (x e2))))))))
    (* (- (sqrt (x d)))
       (sign (y d)))))

(defun-g triangle ((p :vec2) (width :float) (height :float))
  (let ((n (normalize (vec2 height (/ width 2.0)))))
    (max (- (+ (* (x (abs p)) (x n))
               (* (y p) (y n)))
            (* height (y n)))
         (- (y p)))))

(defun-g pie ((p :vec2) (angle :float))
  (let* ((angle (/ angle 2.0))
         (n (vec2 (cos angle) (sin angle))))
    (+ (* (x (abs p)) (x n))
       (* (y p) (y n)))))

(defun-g semicircle ((p :vec2)
                     (radius :float)
                     (angle :float)
                     (line-width :float))
  (let* ((line-width (/ line-width 2f0))
         (radius (- radius line-width)))
    (subtract (pie p angle)
              (- (abs (circle p radius)) line-width))))

(defun-g rectangle ((p :vec2) (size :vec2) (corner-radius :float))
  (let* ((size (- size (vec2 corner-radius)))
         (d (- (abs p) size)))
    (- (+ (min (max (x d) (y d)) 0.0)
          (length (max d 0.0)))
       corner-radius)))

(defun-g rectangle ((p :vec2) (size :vec2))
  (let* ((d (- (abs p) size)))
    (+ (min (max (x d) (y d)) 0.0)
       (length (max d 0.0)))))

(defun-g fast-rect ((p :vec2) (size :vec2))
  (vmax (- (abs p) size)))

(defun-g hexagon ((p :vec2) (h :float))
  (let* ((q (abs p)))
    (- (max (+ (* (x q) 0.866025)
               (* (y q) 0.5))
            (y q))
       h)))

(defun-g line ((p :vec2) (start :vec2) (end :vec2) (line-width :float))
  (let* ((dir (- start end))
         (length (length dir))
         (dir (/ dir length))
         (proj (* (max 0.0 (min length (dot (- start p) dir)))
                  dir)))
    (- (length (- start p proj))
       (/ line-width 2.0))))

;;------------------------------------------------------------

(defun-g rotate-ccw ((p :vec2) (angle :float))
  (let ((m (mat2 (cos angle) (sin angle)
                 (- (sin angle)) (cos angle))))
    (* m p)))

(defun-g translate ((p :vec2) (offset :vec2))
  (- p offset))

;;------------------------------------------------------------
