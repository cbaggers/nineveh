(in-package #:nineveh.tonemapping)

;; Filmic Tonemapping Operators
;;
;; Please see here http://filmicgames.com/archives/75 for excellent explanations
;; of the behaviours of the following
;;

;; Linear
(defun-g tone-map-linear ((color :vec3) (exposure :float))
  (let ((col (* color exposure)))
    (pow col (v3! (/ 1 2.2)))))

;; Reinhard
(defun-g tone-map-reinhard ((color :vec3) (exposure :float))
  (let* ((col (* color exposure))
         (r (/ col (+ (v3! 1) col))))
    (pow r (v3! (/ 1 2.2)))))

;; haarm-peter-duiker
;; The texture film-lut refers to this TGA file:
;; http://filmicgames.com/Downloads/FilmicTonemapping/FilmLut.tga
;; No pow(1/2.2) necessary.
(defun-g tone-map-haarm-peter-duiker ((color :vec3) (exposure :float)
                                      &uniform (film-lut :sampler-2d))
  (let* ((col (* color exposure))
         (ld (v3! 0.002))
         (lin-reference 0.18)
         (log-reference 444)
         (log-gamma 0.45)
         (log-color (/ (+ (* (/ (log10 (* 0.4 (/ col lin-Reference)))
                                ld)
                             log-gamma)
                          (v3! log-reference))
                       1023s0))
         (clamped-log-col (max (v3! 0) (min (v3! 1) log-color)))
         (film-lut-width 256.0)
         (padding (/ 0.5 film-lut-width))
         (r-coord (v! (mix padding (- 1 padding) (x log-color)) 0.5))
         (g-coord (v! (mix padding (- 1 padding) (y log-color)) 0.5))
         (b-coord (v! (mix padding (- 1 padding) (z log-color)) 0.5)))
    (v! (x (texture film-lut r-coord))
        (x (texture film-lut g-coord))
        (x (texture film-lut b-coord)))))

;; Jim Hejl and Richard Burgess-Dawson.
;; No pow(1/2.2) necessary.
(defun-g tone-map-hejl-burgess-dawson ((color :vec3) (exposure :float))
  (let* ((col (* color exposure))
         (x (max (v3! 0) (- col (v3! 0.004)))))
    (/ (* x (+ (* 6.2 x) (v3! 0.5)))
       (+ (* x (+ (* 6.2 x) (v3! 1.7))) (v3! 0.06)))))

;; Uncharted 2 operator (John Hable)
(defun-g tone-map-uncharted2 ((color :vec3) (exposure :float)
                              (exposure-bias :float))
  (let ((a 0.15)
        (b 0.50)
        (c 0.10)
        (d 0.20)
        (e 0.02)
        (f 0.30)
        (w 11.2))
    (labels ((u2-tonemap ((x :vec3))
               (- (/ (+ (* x (+ (* a x) (v3! (* c b))))
                        (v3! (* d e)))
                     (+ (* x (+ (* a x) (v3! b)))
                        (v3! (* d f))))
                  (v3! (/ e f)))))
      (let* ((col (* color exposure))
             (curr (u2-tonemap (* exposure-bias col)))
             (lin-col (* curr (/ (v3! 1) (u2-tonemap (v3! w))))))
        (pow lin-col (v3! (/ 1 2.2)))))))
