(in-package :nineveh.color)

;;------------------------------------------------------------
;;
;; Props to http://blog.demofox.org/2014/02/03/converting-rgb-to-grayscale/
;; for the clear explanation of the formula and why it's needed
;;
;;------------------------------------------------------------

(defun-g rgb->greyscale ((rgb :vec3))
  (+ (* (x rgb) 0.3)
     (* (y rgb) 0.59)
     (* (z rgb) 0.11)))

;;------------------------------------------------------------
;;
;; Mad ♥ to Ian Taylor of http://www.chilliant.com
;; Sam Hocevar of http://lolengine.net
;; and Emil Persson of http://www.humus.name/
;; for doing all the hard work of bringing the following functions to life at
;; http://www.chilliant.com/rgb2hsv.html
;;
;;------------------------------------------------------------

(defconstant +hcx-epsilon+ 1e-10)

;; --- Hue ---

(defun-g hue->rgb ((hue :float))
  (saturate
   (v! (- (abs (- (* hue 6) 3)) 1)
       (- 2 (abs (- (* hue 6) 2)))
       (- 2 (abs (- (* hue 6) 4))))))

;; --- HCV ---

(defun-g rgb->hcv ((rgb :vec3))
  (let* (
         (p (if (< (y rgb) (z rgb))
                (v! (s~ rgb :zy) (- 1.0) (/ 2.0 3.0))
                (v! (s~ rgb :yz) 0.0 (/ (- 1.0) 3.0))))
         (q (if (< (x rgb) (x p))
                (v! (s~ p :xyw) (x rgb))
                (v! (x rgb) (s~ p :yzx))))
         (c (- (x q)
               (min (w q) (y q))))
         (h (abs (+ (/ (- (w q) (y q))
                       (+ (* 6 c) +hcx-epsilon+))
                    (z q)))))
    (v! h c (x q))))

;; --- HSV ---

(defun-g rgb->hsv ((rgb :vec3))
  (let* ((hcv (rgb->hcv rgb))
         (s (/ (y hcv)
               (+ (z hcv) +hcx-epsilon+))))
    (v! (x hcv) s (z hcv))))

(defun-g hsv->rgb ((hsv :vec3))
  (let* ((rgb (hue->rgb (x hsv))))
    (* (+ (* (- rgb (v3! 1f0)) (y hsv)) (v3! 1f0))
       (z hsv))))

;; --- HSL ---

(defun-g rgb->hsl ((rgb :vec3))
  (let* ((hcv (rgb->hcv rgb))
         (l (- (z hcv) (* (y hcv) 0.5)))
         (s (/ (y hcv) (- 1 (+ (abs (- (* l 2) 1))
                               +hcx-epsilon+)))))
    (v! (x hcv) s l)))

(defun-g hsl->rgb ((hsl :vec3))
  (let* ((rgb (hue->rgb (x hsl)))
         (c (* (- (v3! 1)
                  (v3! (abs (- (* 2f0 (z hsl)) 1f0))))
               (y hsl))))
    (+ (* (- rgb (v3! 0.5)) c)
       (v3! (z hsl)))))

;; --- HCY ---

(defun-g rgb->hcy ((rgb :vec3))
  (let* ((hcy-wts (v! 0.299 0.587 0.114))
         (hcv (rgb->hcv rgb))
         (y (dot rgb hcy-wts))
         (z
          (dot (hue->rgb (x hcv))
               hcy-wts)))
    (if (< y z)
        (multf (y hcv)
               (/ z (+ +hcx-epsilon+ y)))
        (multf (y hcv)
               (/ (- 1 z)
                  (- (+ +hcx-epsilon+ 1) y))))
    (v! (x hcv) (y hcv) y)))

(defun-g hcy->rgb ((hcy :vec3))
  (let* ((hcy-wts (v! 0.299 0.587 0.114))
         (rgb (hue->rgb (x hcy)))
         (z (dot rgb hcy-wts)))
    (if (< (z hcy) z)
        (multf (y hcy) (/ (z hcy) z))
        (when (< z 1)
          (multf (y hcy) (/ (- 1 (z hcy)) (- 1 z)))))
    (+ (* (- rgb (v3! z)) (y hcy))
       (v3! (z hcy)))))

;; --- HCL ---

(defun-g rgb->hcl ((rgb :vec3))
  (let* ((hcl-gamma 3f0)
         (hcl (v3! 0))
         (hcl-y0 100f0)
         (hclmax-l "0.530454533953517")
         (h 0f0)
         (u (min (x rgb) (min (y rgb) (z rgb))))
         (v (max (x rgb) (max (y rgb) (z rgb))))
         (q (/ hcl-gamma hcl-y0)))
    (setf (y hcl) (- v u))
    (when (/= (y hcl) 0)
      (setf h (/ (atan (- (x rgb) (y rgb)) (- (y rgb) (z rgb))) pi-f))
      (multf q (/ u v)))
    (setf q (exp q))
    (setf (x hcl) (fract (- (/ h 2) (/ (min (fract h) (fract (- h))) 6))))
    (multf (y hcl) q)
    (setf (z hcl) (/ (mix (- u) v q) (* hclmax-l 2)))
    hcl))

;; Compiles to bad glsl -
;; (defun-g hcl->rgb ((hcl :vec3))
;;   (let* ((hcl-gamma 3f0)
;;          (hcl-y0 100f0)
;;          (hclmax-l "0.530454533953517")
;;          (rgb (v3! 0)))
;;     (when (/= (z hcl) 0)
;;       (let* ((h (x hcl))
;;              (c (y hcl))
;;              (l (* (z hcl) hclmax-l))
;;              (q (exp (* (- 1 (/ c (* 2 l))) (/ hcl-gamma hcl-y0))))
;;              (u (/ (- (* 2 l) c) (- (* 2 q) 1)))
;;              (v (/ c q))
;;              (tx
;;               (tan
;;                (* (+ h (min (/ (fract (* 2 h)) 4) (/ (fract (* (- 2) h)) 8)))
;;                   (* pi-f 2)))))
;;         (multf h 6f0)
;;         (cond
;;           ((<= h 1f0)
;;            (setf (x rgb) 1f0)
;;            (setf (y rgb) (/ tx (+ 1f0 tx)))
;;            (values))
;;           ((<= h 2)
;;            (setf (x rgb) (/ (+ 1f0 tx) tx))
;;            (setf (y rgb) 1f0)
;;            (values))
;;           ((<= h 3)
;;            (setf (y rgb) 1f0)
;;            (setf (z rgb) (+ 1f0 tx))
;;            (values))
;;           ((<= h 4)
;;            (setf (y rgb) (/ 1f0 (+ 1f0 tx)))
;;            (setf (z rgb) 1f0)
;;            (values))
;;           ((<= h 5)
;;            (setf (x rgb) (/ (- 1f0) tx))
;;            (setf (z rgb) 1f0)
;;            (values))
;;           (t
;;            (setf (x rgb) 1f0)
;;            (setf (z rgb) (- tx))
;;            (values)))
;;         (setf rgb (+ (* rgb v) (v3! u)))))
;;     rgb))

;;------------------------------------------------------------
;; http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl is also worth reading
;; The site seems worryingly slow so in-case it has gone here is the wayback
;; version: https://web.archive.org/web/20161105155520/http://lolengine.net/blog/2013/07/27/rgb-to-hsv-in-glsl
;;------------------------------------------------------------
