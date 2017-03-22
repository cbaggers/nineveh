(in-package :nineveh)

;; stateless version of chromatic aberration ramp function (ferris)
;; vec3 aberrationColor(float f)
;; {
;;     f *= 3.0;

;;     float r = 1.0 - (f - 0.5);
;;     float g = 1.0 - abs(f - 1.5);
;;     float b = f - 1.5;

;;     return saturate(vec3(r, g, b));
;; }
(defun-g aberration-color-ramp-stateless ((f :float))
  (let* ((f (* f 3f0))
         (r (- 1f0 (- f 0.5)))
         (g (- 1f0 (abs (- f 1.5))))
         (b (- f 1.5)))
    (saturate (v! r g b))))
