(in-package :nineveh.color)

;; Props to http://blog.demofox.org/2014/02/03/converting-rgb-to-grayscale/
;; for the clear explanation of the formula and why it's needed
;;
(defun-g rgb->greyscale ((rgb :vec3))
  (+ (* (x rgb) 0.3)
     (* (y rgb) 0.59)
     (* (z rgb) 0.11)))


;; Fast branchless RGB to HSV conversion in GLSL
;; Impl translated from http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
;; The site seems worryingly slow so in-case it has gone here is the wayback
;; version: https://web.archive.org/web/20161105155520/http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
;;
;; {TODO} ​Emil Persson suggests using the ternary operator explicitly to force
;;        compilers into using a fast conditional move instruction:
;;
;;     vec4 p = c.g < c.b ? vec4(c.bg, K.wz) : vec4(c.gb, K.xy);
;;     vec4 q = c.r < p.x ? vec4(p.xyw, c.r) : vec4(c.r, p.yzx);
;;
;; Test this
(defun-g rgb->hsv ((rgb :vec3))
  (let* ((k (v! 0f0 (/ -1f0 3f0) (/ 2f0 3f0) -1f0))
         (p (mix (v! (s~ rgb :zy) (s~ k :wz))
                 (v! (s~ rgb :yz) (s~ k :xy))
                 (step (z rgb) (y rgb))))
         (q (mix (v! (s~ p :xyw) (x rgb))
                 (v! (x rgb) (s~ k :yzx))
                 (step (x p) (x rgb))))
         (d (- (x q) (min (w q) (y q))))
         (e 1.0e-10))
    (v! (abs (+ (/ (- (w q) (y q))
                   (+ (* 6f0 d) e))
                (z q)))
        (/ d (+ (x q) e))
        (x q))))


(defun-g hsv->rgb ((hsv :vec3))
  (let* ((k (v! 1f0 (/ 2f0 3f0) (/ 1f0 3f0) 3f0))
         (p (abs (- (* (fract (+ (s~ hsv :xxx) (s~ k :xyz))) 6f0)
                    (s~ k :www)))))
    (* (z hsv)
       (mix (s~ k :xxx)
            (saturate (- p (s~ k :xxx)))
            (y hsv)))))
